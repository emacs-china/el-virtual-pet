(defun vp--insert-image-button (label &optional img &rest properties)
  "insert a button that displayed as the `IMG'"
  (let ((btn (apply #'insert-text-button label properties)))
    (when img
      (add-text-properties (button-start btn) (button-end btn)
                           `(display ,(create-image img))))))

(defun vp--insert-image-function-button (label img fn)
  "insert a image button which when clicked will triggle `FN'"
  (vp--insert-image-button label img 'follow-link t 'action fn))

(defun vp--insert-image (label &optional img width height)
  "insert a `IMG' with specified width and height"
  (let ((pos-start (point))
        pos-end)
    (insert label)
    (setq pos-end (point))
    (when img
      (if (or width height)
          (add-text-properties pos-start pos-end `(display ,(create-image img 'imagemagick nil :width width :height height)))
        (add-text-properties pos-start pos-end `(display ,(create-image img)))))))

(defun vp--insert-process-bar (label current max)
  "insert a process bar"
  (let ((process-bar (format "%-10s:%s%s" label (make-string current ?*) (make-string (- max current) ?\ ))))
    (insert process-bar)))


(defcustom vp-buffer "*virtual-pet*"
  "name of virtual pet buffer")

(defstruct virtual-pet
  (name (read-string "please input the pet's name:"))
  (hunger 0)
  (happiness 20)
  (health 20)
  (status 'stop))

(defvar vp-the-pet (make-virtual-pet))

(defun vp--change-pet-status (pet status)
  (setf (virtual-pet-status pet) status)
  (vp--redraw-pet pet))

(defun vp--redraw-action-buttons ()
  "redraw action buttons in the first row"
  (with-current-buffer (get-buffer-create vp-buffer)
    (goto-char (point-min))
    (ignore-errors 
      (kill-whole-line))
    (dolist (action '("stop" "sick" "play" "walk" "eat"))
      (insert "\t")
      (let ((btn-img (format "%s%s-button.GIF" default-directory action)))
        (vp--insert-image-function-button action btn-img (lambda (btn)
                                                           (vp--change-pet-status vp-the-pet (intern (button-label btn)))
                                                           (vp--redraw-pet vp-the-pet)))))
    (newline)))

(defun vp--redraw-pet (pet)
  "redraw pet image in the second row"
  (with-current-buffer (get-buffer-create vp-buffer)
    (goto-char (point-min))
    (forward-line)
    (ignore-errors
      (kill-whole-line))
    (vp--insert-image (symbol-name (virtual-pet-status pet))
                      (format "%s%s.jpg" default-directory (virtual-pet-status pet))
                      450 348)
    (newline)))


(defun vp--redraw-pet-status (pet)
  "redraw pet status process bar in the third row"
  (with-current-buffer (get-buffer-create vp-buffer)
    (goto-char (point-min))
    (forward-line)
    (forward-line)
    (delete-region (point) (point-max))
    (vp--insert-process-bar "hunger" (virtual-pet-hunger pet) 20)
    (newline)
    (vp--insert-process-bar "health" (virtual-pet-health pet) 20)
    (newline)
    (vp--insert-process-bar "happiness" (virtual-pet-happiness pet) 20)))


(defun vp-redraw-gui ()
  "redraw the gui"
  (switch-to-buffer (get-buffer-create vp-buffer))
  (vp--redraw-action-buttons)
  (vp--redraw-pet vp-the-pet)
  (vp--redraw-pet-status vp-the-pet))

(defvar vp--current-time-cycle 0)

(defgroup virtual-pet nil
  "virtual pet customize group")

(defcustom vp-seconds-per-time-cycle 5
  "")
(defcustom vp-time-cycle-per-day 60
  "")
(defcustom vp-time-cycle-to-sleep 48
  "")
(defcustom vp-status-change-rule-alist '((stop (1 . 1)  ;hunger
                                               (2 . -1) ;happiness
                                               )
                                         (sleep (3 . 1) ;hunger
                                                )
                                         (eat (1 . -1) ;hunger
                                              )
                                         (play (1 . 1) ;hunger
                                               (1 . 1) ;happiness
                                               )
                                         (walk (1 . 1) ;hunger
                                               (2 . 1) ;happiness
                                               (2 . 1) ;health
                                               )
                                         (sick (1 . 1)  ;hunger
                                               (2 . -1) ;happiness
                                               (1 . 1)  ;health
                                               ))
  "")

(defun vp--incf-pet-hunger (pet change)
  (let* ((hunger (virtual-pet-hunger pet))
         (new-hunger (min 20 (+ hunger change)))
         (new-hunger (max 0 new-hunger)))
    (setf (virtual-pet-hunger pet) new-hunger )))

(defun vp--incf-pet-happiness (pet change)
  (let ((happiness (virtual-pet-happiness pet))
        (new-happiness (min 20 (+ happiness change)))
        (new-happiness (max 0 new-happiness)))
    (setf (virtual-pet-happiness pet) new-happiness)))

(defun vp--incf-pet-health (pet change)
  (let* ((health (virtual-pet-health pet))
         (new-health (min 20 (+ health change)))
         (new-health (max 0 new-health)))
    (setf (virtual-pet-health pet) new-health)))


(defun vp--change-status (pet status-change-rule-alist)
  (let* ((status (virtual-pet-status pet))
         (change (cdr (assoc status status-change-rule-alist)))
         (hunger-change (first change))
         (happiness-change (second change))
         (health-change (third change)))
    (when (and hunger-change
               (= 0 (% vp--current-time-cycle (car hunger-change))))
      (vp--incf-pet-hunger pet (cdr hunger-change)))
    (when (and happiness-change
               (= 0 (% vp--current-time-cycle (car happiness-change))))
      (vp--incf-pet-happiness pet (cdr happiness-change)))
    (when (and health-change
               (= 0 (% vp--current-time-cycle (car health-change))))
      (vp--incf-pet-health pet (cdr health-change)))
    (vp--redraw-pet-status pet))
  (setf vp--current-time-cycle (% (+ 1 vp--current-time-cycle) vp-time-cycle-per-day))
  (when (= vp--current-time-cycle vp-time-cycle-to-sleep)
    (vp--change-pet-status pet 'sleep))
  (when (and (= vp--current-time-cycle 0)
             (eq 'sleep (virtual-pet-status pet)))
    (vp--change-pet-status pet 'stop)))

(defun vp-change-status ()
  (interactive)
  (vp--change-status vp-the-pet vp-status-change-rule-alist))

(defvar vp-timer nil)
(defun vp-start-game ()
  (interactive)
  (vp-redraw-gui)
  (when (timerp vp-timer)
    (cancel-timer vp-timer))
  (setq vp-timer (run-with-timer 0 vp-seconds-per-time-cycle #'vp-change-status)))

(defun vp-stop-game ()
  (interactive)
  (when (timerp vp-timer)
    (cancel-timer vp-timer)))

(provide 'virtual-pet)
