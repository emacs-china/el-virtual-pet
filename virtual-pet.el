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

(defun vp--redraw-action-buttons ()
  "redraw action buttons in the first row"
  (goto-char (point-min))
  (ignore-errors 
    (kill-whole-line))
  (dolist (action '("stop" "sick" "play" "walk" "eat"))
    (insert "\t")
    (let ((btn-img (format "%s%s-button.GIF" default-directory action)))
      (vp--insert-image-function-button action btn-img (lambda (btn)
                                                         (setf (virtual-pet-status vp-the-pet)
                                                               (intern (button-label btn)))
                                                         (vp--redraw-pet vp-the-pet)))))
  (newline))

(defun vp--redraw-pet (pet)
  "redraw pet image in the second row"
  (goto-char (point-min))
  (forward-line)
  (ignore-errors
    (kill-whole-line))
  (vp--insert-image (symbol-name (virtual-pet-status pet))
                    (format "%s%s.jpg" default-directory (virtual-pet-status pet))
                    450 348)
  (newline))


(defun vp--redraw-pet-status (pet)
  "redraw pet status process bar in the third row"
  (goto-char (point-min))
  (forward-line)
  (forward-line)
  (delete-region (point) (point-max))
  (vp--insert-process-bar "hunger" (virtual-pet-hunger vp-the-pet) 20)
  (newline)
  (vp--insert-process-bar "health" (virtual-pet-health vp-the-pet) 20)
  (newline)
  (vp--insert-process-bar "happiness" (virtual-pet-happiness vp-the-pet) 20))


(defun vp-redraw-gui ()
  "redraw the gui"
  (switch-to-buffer (get-buffer-create vp-buffer))
  (vp--redraw-action-buttons)
  (vp--redraw-pet vp-the-pet)
  (vp--redraw-pet-status vp-the-pet))


(vp-redraw-gui)
