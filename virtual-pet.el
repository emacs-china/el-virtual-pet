;; GUI API
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

(defun vp--redraw-action-buttons ()
  "redraw action buttons in the first row"
  (with-current-buffer (get-buffer-create vp-buffer)
    (goto-char (point-min))
    (ignore-errors 
      (kill-whole-line))
    (dolist (action '("stop" "sick" "play" "walk" "eat"))
      (insert "\t")
      (let ((btn-img (format "%s%s-button.GIF" default-directory action)))
        (vp--insert-image-function-button action
                                          btn-img
                                          (lambda (btn)
                                            (let ((current-status (virtual-pet-status vp-the-pet))
                                                  (new-status (intern (button-label btn)))))
                                            ;; 如果睡觉被叫醒,快乐度-4
                                            (when (vp-pet-sleep-p vp-the-pet)
                                              (message "You waked %s up so he is unhappy" (virtual-pet-name vp-the-pet))
                                              (vp--incf-pet-happiness vp-the-pet -4))
                                            (vp--change-pet-status vp-the-pet new-status)))))
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
    (vp--insert-process-bar "hunger" (virtual-pet-hunger pet) vp-max-hunger)
    (newline)
    (vp--insert-process-bar "health" (virtual-pet-health pet) vp-max-heath)
    (newline)
    (vp--insert-process-bar "happiness" (virtual-pet-happiness pet) vp-max-happiness)))


(defun vp-redraw-gui ()
  "redraw the gui"
  (switch-to-buffer (get-buffer-create vp-buffer))
  (vp--redraw-action-buttons)
  (vp--redraw-pet vp-the-pet)
  (vp--redraw-pet-status vp-the-pet))


(defgroup virtual-pet nil
  "virtual pet customize group")

(defcustom vp-buffer "*virtual-pet*"
  "name of virtual pet buffer"
  :group 'virtual-pet)

(defcustom vp-max-hunger 20
  "max health point"
  :group 'virtual-pet)

(defcustom vp-max-happiness 20
  "max health point"
  :group 'virtual-pet)

(defcustom vp-max-heath 20
  "max health point"
  :group 'virtual-pet)

(defstruct virtual-pet
  (name (read-string "please input the pet's name:"))
  (hunger 0)
  (happiness vp-max-happiness)
  (health vp-max-heath)
  (status 'stop))

(defvar vp-the-pet (make-virtual-pet))

(defun vp--change-pet-status (pet status)
  (setf (virtual-pet-status pet) status)
  (vp--redraw-pet pet))

(defvar vp--current-time-cycle 0)

(defcustom vp-seconds-per-time-cycle 5
  ""
  :group 'virtual-pet)
(defcustom vp-time-cycle-per-day 60
  ""
  :group 'virtual-pet)
(defcustom vp-time-cycle-to-sleep 48
  ""
  :group 'virtual-pet)
(defcustom vp-status-change-rule-alist '((stop (1 . 1)  ;醒着时,饥饿度每个滴答+1
                                               (2 . -1) ;快乐度每2个滴答-1
                                               )
                                         (sleep (3 . 1) ;睡觉时,饥饿感每3个滴答+1
                                                )
                                         (eat (1 . -1) ;进食时,饥饿感每个滴答-1
                                              )
                                         (play (1 . 1) ;玩时,饥饿感每个滴答+1
                                               (1 . 1) ;快乐度每个滴答+1
                                               )
                                         (walk (1 . 1) ;散步时,饥饿感每个滴答+1
                                               (2 . 1) ;快乐感每两个滴答+1
                                               (2 . 1) ;健康度每两个滴答+1
                                               )
                                         (sick (1 . 1)  ;看病时,饥饿度每个滴答+1
                                               (2 . -1) ;快乐度每2个滴答-1
                                               (1 . 1)  ;健康度每个滴答+1
                                               ))
  ""
  :group 'virtual-pet)

(defun vp--incf-pet-hunger (pet change)
  (let* ((hunger (virtual-pet-hunger pet))
         (new-hunger (min vp-max-hunger (+ hunger change)))
         (new-hunger (max 0 new-hunger)))
    (setf (virtual-pet-hunger pet) new-hunger )))

(defun vp--incf-pet-happiness (pet change)
  (let* ((happiness (virtual-pet-happiness pet))
         (new-happiness (min vp-max-happiness (+ happiness change)))
         (new-happiness (max 0 new-happiness)))
    (setf (virtual-pet-happiness pet) new-happiness)))

(defun vp--incf-pet-health (pet change)
  (let* ((health (virtual-pet-health pet))
         (new-health (min vp-max-heath (+ health change)))
         (new-health (max 0 new-health)))
    (setf (virtual-pet-health pet) new-health)))

(defun vp--pet-dead-p (pet)
  "wether pet dead or not"
  (= 0 (virtual-pet-health pet)))

(defun vp--time-passed-by ()
  (setf vp--current-time-cycle (% (+ 1 vp--current-time-cycle) vp-time-cycle-per-day)))

(defun vp--reset-pet-status (pet time-cycle)
  "reset status of PET according TIME-CYCLE"
  ;; reset pet status
  (when (= time-cycle vp-time-cycle-to-sleep)
    (vp--change-pet-status pet 'sleep))
  (when (and (= time-cycle 0)
             (eq 'sleep (virtual-pet-status pet)))
    (vp--change-pet-status pet 'stop)))

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
      (vp--incf-pet-health pet (cdr health-change))))
  ;; 当饥饿度到达最大的80%,健康度每2个滴答-1
  (when (and (>= (virtual-pet-hunger pet) (* vp-max-hunger 0.8))
             (< (virtual-pet-hunger pet) vp-max-hunger)
             (= 0 (% vp--current-time-cycle 2)))
    (vp--incf-pet-health pet -1))
  ;; 当饥饿度到达最大时,健康度每个滴答-1
  (when (= (virtual-pet-hunger pet) vp-max-hunger)
    (vp--incf-pet-health pet -1)))


(defun vp-change-status ()
  (interactive)
  (vp--change-status vp-the-pet vp-status-change-rule-alist)
  (vp--redraw-pet-status vp-the-pet)
  (vp--time-passed-by)
  (vp--reset-pet-status vp-the-pet vp--current-time-cycle)
  (when (vp--pet-dead-p vp-the-pet)
    (message "sorry, %s is dead %s" (virtual-pet-name vp-the-pet) (if (> (virtual-pet-happiness vp-the-pet) (* 0.5 vp-max-happiness)) "peaceful" "painful"))
    (vp-stop-game)))

(defvar vp-timer nil)

;;;###autoload
(defun vp-start-game ()
  (interactive)
  (vp-redraw-gui)
  (when (timerp vp-timer)
    (cancel-timer vp-timer))
  (setq vp-timer (run-with-timer 0 vp-seconds-per-time-cycle #'vp-change-status)))

;;;###autoload
(defun vp-stop-game ()
  (interactive)
  (when (timerp vp-timer)
    (cancel-timer vp-timer)))

(provide 'virtual-pet)
