(in-package :cube-viewer)

(sera:-> make-scale
         ((member :horizontal :vertical)
          double-float double-float double-float double-float)
         (values gtk:gtk-scale &optional))
(defun make-scale (orientation value start stop step)
  (make-instance 'gtk:gtk-scale
                 :orientation orientation
                 :hexpand     t
                 :halign      :fill
                 :digits      3
                 :adjustment  (make-instance 'gtk:gtk-adjustment
                                             :value          value
                                             :lower          start
                                             :upper          stop
                                             :step-increment step)))

(sera:-> make-color-button
         ((simple-array double-float (3)))
         (values gtk:gtk-color-button &optional))
(defun make-color-button (color)
  (make-instance 'gtk:gtk-color-button
                 :rgba (gdk:make-gdk-rgba :red   (aref color 0)
                                          :green (aref color 1)
                                          :blue  (aref color 2)
                                          :alpha 1d0)))

(sera:-> update-camera-position
         (camera)
         (values camera &optional))
(defun update-camera-position (camera)
  (let ((ϕ (camera-ϕ camera))
        (ψ (camera-ψ camera))
        (dist (camera-dist camera)))
    (setf (camera-position camera)
          (rtg-math.vector3:make
           (float (* dist (cos ψ) (cos ϕ)) 0.0)
           (float (* dist (sin ψ)) 0.0)
           (float (* dist (cos ψ) (sin ϕ)) 0.0)))
    camera))

(defun run (name side)
  "Run cube viewer. NAME is a name of raw file with XCT
densities. SIDE is a side of cubic array stored in that file. Elements
of the array must be 8 bit unsigned values."
  (declare (type (or pathname string) name)
           (type alex:positive-fixnum side))
  (let ((density-data (load-data name side side side))
        (gl-state (make-gl-state))
        (camera (make-camera)))
    (gtk:within-main-loop
      (let ((window (make-instance 'cube-viewer-window
                                   :type      :toplevel
                                   :cube-name (pathname-name (pathname name))))
            (area (make-instance 'gtk:gtk-gl-area))
            (threshold-scale (make-scale :horizontal
                                         (gl-state-threshold gl-state)
                                         0d0 1d0 5d-2))
            (scale-scale (make-scale :horizontal
                                     (gl-state-scale gl-state)
                                     0d0 1d0 5d-2))
            (ϕ-scale (make-scale :horizontal
                                 (camera-ϕ camera)
                                 0d0 (* 2 pi) 1d-1))
            (ψ-scale (make-scale :horizontal
                                 (camera-ψ camera)
                                 (- (/ pi 2)) (+ (/ pi 2)) 1d-1))
            (solid-color (make-color-button (gl-state-solid-color gl-state)))
            (void-color  (make-color-button (gl-state-void-color  gl-state)))
            (save-button (make-instance 'gtk:gtk-button :label "Save image")))
        (flet ((scale-handler (scale)
                 (let ((value (gtk:gtk-range-get-value scale)))
                   (cond
                     ((eq scale threshold-scale)
                      (setf (gl-state-threshold gl-state) value))
                     ((eq scale scale-scale)
                      (setf (gl-state-scale gl-state) value))
                     ((eq scale ϕ-scale)
                      (setf (camera-ϕ camera) value)
                      (update-camera-position camera))
                     ((eq scale ψ-scale)
                      (setf (camera-ψ camera) value)
                      (update-camera-position camera))
                     (t (error "This never happens"))))
                 (gtk:gtk-gl-area-queue-render area))
               (color-button-handler (button)
                 (let* ((color (gtk:gtk-color-chooser-get-rgba button))
                        (vector (dvec3 (gdk:gdk-rgba-red   color)
                                       (gdk:gdk-rgba-green color)
                                       (gdk:gdk-rgba-blue  color))))
                   (cond
                     ((eq button solid-color)
                      (setf (gl-state-solid-color gl-state) vector))
                     ((eq button void-color)
                      (setf (gl-state-void-color gl-state) vector))
                     (t (error "This never happens"))))
                 (gtk:gtk-gl-area-queue-render area)))
          (mapc
           (alex:rcurry
            #'gobject:g-signal-connect
            "value-changed"
            #'scale-handler)
           (list threshold-scale scale-scale ϕ-scale ψ-scale))
          (mapc
           (alex:rcurry
            #'gobject:g-signal-connect
            "color-set"
            #'color-button-handler)
           (list solid-color void-color)))

        (gobject:g-signal-connect
         window "destroy"
         (lambda (widget)
           (declare (ignore widget))
           (gtk:leave-gtk-main)))
        (gobject:g-signal-connect
         area "render"
         (lambda (area context)
           (declare (ignore context))
           (draw-cube area gl-state camera)
           nil))
        (gobject:g-signal-connect
         area "realize"
         (lambda (widget)
           (declare (ignore widget))
           (prepare-rendering area gl-state density-data)))
        (gobject:g-signal-connect
         area "unrealize"
         (lambda (widget)
           (declare (ignore widget))
           (cleanup area gl-state)))
        (gobject:g-signal-connect
         save-button "clicked"
         (lambda (widget)
           (declare (ignore widget))
           (image-save-dialog
            window (alex:curry #'save-image area))))

        (update-camera-position camera)
        (let ((box (make-instance 'gtk:gtk-box :orientation :vertical)))
          (gtk:gtk-box-pack-start box area)
          (let ((control-grid (make-instance 'gtk:gtk-grid)))
            (gtk:gtk-grid-attach
             control-grid (make-instance 'gtk:gtk-label :label "Threshold")
             0 0 1 1)
            (gtk:gtk-grid-attach control-grid threshold-scale 0 1 1 1)
            (gtk:gtk-grid-attach
             control-grid (make-instance 'gtk:gtk-label :label "Scale")
             0 2 1 1)
            (gtk:gtk-grid-attach control-grid scale-scale 0 3 1 1)
            (gtk:gtk-grid-attach
             control-grid (make-instance 'gtk:gtk-label :label "Φ")
             1 0 1 1)
            (gtk:gtk-grid-attach control-grid ϕ-scale 1 1 1 1)
            (gtk:gtk-grid-attach
             control-grid (make-instance 'gtk:gtk-label :label "Ψ")
             1 2 1 1)
            (gtk:gtk-grid-attach control-grid ψ-scale 1 3 1 1)
            (gtk:gtk-grid-attach control-grid solid-color 2 0 1 2)
            (gtk:gtk-grid-attach control-grid void-color  2 2 1 2)
            (gtk:gtk-grid-attach control-grid save-button 0 4 3 1)
            (gtk:gtk-box-pack-end box control-grid :expand nil))
          (gtk:gtk-container-add window box))
        (gtk:gtk-widget-show-all window)))))
