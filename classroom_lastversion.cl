;; Define the structures for each element in the classroom
;  (load "C:/Users/serra/Desktop/VIE_projectSerra/classroom_lastversion.cl")

(defpackage :classroom
  (:use :cl))

(in-package :classroom)

;; Define the structures for each element in the classroom
(defstruct student
  name
  age
  grade
  x
  y
  z
  movable)

(defstruct teacher
  name
  subject
  experience-years
  x
  y
  z)

(defstruct board
  description
  material
  x
  y
  z)

(defstruct clock
  description
  type
  x
  y
  z)

(defstruct window
  description
  status
  x
  y
  z)

(defstruct light
  description
  status)

;; Create instances of students with more attributes
(setq students (list
                (make-student :name "Alice" :age 10 :grade 5 :x 1 :y 1 :z 0 :movable t)
                (make-student :name "Bob" :age 11 :grade 5 :x 2 :y 1 :z 0 :movable nil)
                (make-student :name "Charlie" :age 10 :grade 5 :x 3 :y 1 :z 0 :movable t)
                (make-student :name "David" :age 11 :grade 5 :x 4 :y 1 :z 0 :movable nil)
                (make-student :name "Eva" :age 10 :grade 5 :x 5 :y 1 :z 0 :movable t)
                (make-student :name "Frank" :age 11 :grade 5 :x 1 :y 2 :z 0 :movable t)
                (make-student :name "Grace" :age 10 :grade 5 :x 2 :y 2 :z 0 :movable nil)
                (make-student :name "Hannah" :age 11 :grade 5 :x 3 :y 2 :z 0 :movable t)
                (make-student :name "Ivan" :age 10 :grade 5 :x 4 :y 2 :z 0 :movable nil)))

;; Create an instance of the teacher with more attributes
(setq teacher (make-teacher :name "Mr. Smith" :subject "Math" :experience-years 15 :x 0 :y 0 :z 0))

;; Create an instance of the board with more attributes
(setq classroom-board (make-board :description "Whiteboard" :material "Porcelain" :x 0 :y 4 :z 0))

;; Create an instance of the clock with more attributes
(setq classroom-clock (make-clock :description "Wall Clock" :type "Analog" :x 0 :y 5 :z 0))

;; Create instances of windows with more attributes
(setq windows (list
               (make-window :description "Main Window" :status "Closed" :x 4 :y 3 :z 0)
               (make-window :description "Secondary Window" :status "Closed" :x 5 :y 3 :z 0)))

;; Create instances of lights
(setq lights (list
              (make-light :description "Ceiling Light 1" :status "Off")
              (make-light :description "Ceiling Light 2" :status "Off")))

;; Function to display detailed information about the students
(defun display-students-details ()
  (format t "Students:~%")
  (dolist (student students)
    (format t "  - Name: ~a, Age: ~a, Grade: ~a, Position: (~a, ~a, ~a), Movable: ~a~%" 
            (student-name student) 
            (student-age student) 
            (student-grade student)
            (student-x student)
            (student-y student)
            (student-z student)
            (student-movable student))))

;; Function to display detailed information about the teacher
(defun display-teacher-details ()
  (format t "Teacher:~%")
  (format t "  - Name: ~a, Subject: ~a, Experience: ~a years, Position: (~a, ~a, ~a)~%" 
          (teacher-name teacher) 
          (teacher-subject teacher) 
          (teacher-experience-years teacher)
          (teacher-x teacher)
          (teacher-y teacher)
          (teacher-z teacher)))

;; Function to display detailed information about the board
(defun display-board-details ()
  (format t "Board:~%")
  (format t "  - Description: ~a, Material: ~a, Position: (~a, ~a, ~a)~%" 
          (board-description classroom-board) 
          (board-material classroom-board)
          (board-x classroom-board)
          (board-y classroom-board)
          (board-z classroom-board)))

;; Function to display detailed information about the clock
(defun display-clock-details ()
  (format t "Clock:~%")
  (format t "  - Description: ~a, Type: ~a, Position: (~a, ~a, ~a)~%" 
          (clock-description classroom-clock) 
          (clock-type classroom-clock)
          (clock-x classroom-clock)
          (clock-y classroom-clock)
          (clock-z classroom-clock)))

;; Function to display detailed information about the windows
(defun display-windows-details ()
  (format t "Windows:~%")
  (dolist (window windows)
    (format t "  - Description: ~a, Status: ~a, Position: (~a, ~a, ~a)~%" 
            (window-description window) 
            (window-status window)
            (window-x window)
            (window-y window)
            (window-z window))))

;; Function to display detailed information about the lights
(defun display-lights-details ()
  (format t "Lights:~%")
  (dolist (light lights)
    (format t "  - Description: ~a, Status: ~a~%" 
            (light-description light) 
            (light-status light))))

;; Function to display the entire classroom details upon request
(defun display-classroom-details ()
  (format t "Classroom Details:~%")
  (display-teacher-details)
  (display-students-details)
  (display-board-details)
  (display-clock-details)
  (display-windows-details)
  (display-lights-details))

;; Function to change the status of a window
(defun change-window ()
  (format t "Enter the description of the window (e.g., Main Window): ")
  (let ((window-desc (read-line)))
    (format t "Enter the new status (Open/Closed): ")
    (let ((new-status (read-line)))
      (change-window-status window-desc new-status))))


;; Function to change the status of a light
(defun change-light ()
  (format t "Enter the description of the light (e.g., Ceiling Light 1): ")
  (let ((light-desc (read-line)))
    (format t "Enter the new status (On/Off): ")
    (let ((new-status (read-line)))
      (change-light-status light-desc new-status))))

(defun change-window-status (window-desc new-status)
  (let ((window (find window-desc windows :key #'window-description :test #'string=)))
    (when window
      (setf (window-status window) new-status)
      (format t "The status of ~a has been changed to ~a.~%" 
              window-desc 
              new-status))))

(defun change-light-status (light-desc new-status)
  (let ((light (find light-desc lights :key #'light-description :test #'string=)))
    (when light
      (setf (light-status light) new-status)
      (format t "The status of ~a has been changed to ~a.~%" 
              light-desc
              new-status))))

(defun move-student (student-name new-x new-y new-z)
  (let ((student (find student-name students :key #'student-name :test #'string=)))
    (when student
      (if (student-movable student)
          (progn
            (setf (student-x student) new-x
                  (student-y student) new-y
                  (student-z student) new-z)
            (format t "Student ~a has been moved to Position: (~a, ~a, ~a).~%"
                    student-name new-x new-y new-z))
          (format t "Student ~a cannot be moved.~%" student-name)))))

;; Function to simulate periodic light toggling and student moving
(defun toggle-lights ()
  (dolist (light lights)
    (setf (light-status light)
          (if (string= (light-status light) "Off") "On" "Off"))
    (format t "The status of ~a has been changed to ~a.~%"
            (light-description light)
            (light-status light))))

(defun randomly-move-students ()
  (dolist (student students)
    (when (student-movable student)
      (move-student (student-name student)
                    (+ 1 (random 5))  ; Random x position between 1 and 5
                    (+ 1 (random 5))  ; Random y position between 1 and 5
                    0))))            ; z position remains 0 for simplicity

(defun periodic-actions (start-time interval)
  (let ((current-time (get-universal-time)))
    (when (>= (- current-time start-time) interval)
      (toggle-lights)
      (randomly-move-students)
      (setq start-time current-time)))
  start-time)

(defun main ()
  (format t "Welcome to the Classroom Management System!~%")
  (format t "Type 'exit' to quit the program.~%")

  ;; Initialize variables for periodic actions
  (let ((start-time (get-universal-time))
        (interval 2)  ; Interval in seconds for periodic actions
        (previous-students (copy-list students))  ; Keep track of previous state
        (previous-lights (copy-list lights)))     ; of students and lights

    ;; Main loop
    (loop
      ;; Perform periodic actions every interval seconds
      (sleep interval)
      (toggle-lights)
      (randomly-move-students)

      ;; Check for changes in movable objects (students) and lights
      (let ((changed-students (get-changed-elements previous-students students #'equalp))
            (changed-lights (get-changed-elements previous-lights lights #'equalp)))
        (when (or changed-students changed-lights)
          (format t "Changes in movable objects and lights:~%")
          (when changed-students
            (format t "Students:~%")
            (dolist (student changed-students)
              (format t "  - Name: ~a, Position: (~a, ~a, ~a)~%"
                      (student-name student)
                      (student-x student)
                      (student-y student)
                      (student-z student))))
          (when changed-lights
            (format t "Lights:~%")
            (dolist (light changed-lights)
              (format t "  - Description: ~a, Status: ~a~%"
                      (light-description light)
                      (light-status light)))))

        ;; Update previous state
        (setf previous-students (copy-list students))
        (setf previous-lights (copy-list lights))

        ;; Check for user input
        (format t "Enter your command (exit to quit):~%")
        (let ((input (read-line)))
          (cond
            ((string= input "exit") (return))
            ((string= input "details") (display-classroom-details))
            ((string= input "window") (change-window))
            ((string= input "light") (change-light))
            ((string= input "move") (move-student-function))
            (t (format t "Invalid command. Try again.~%"))))))))

(defun get-changed-elements (previous current test)
  "Returns a list of elements in CURRENT that are not in PREVIOUS using TEST for comparison."
  (remove-if-not (lambda (elem)
                   (not (member elem previous :test test)))
                 current))


(main)
