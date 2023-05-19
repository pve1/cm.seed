(package.seed:define-seed-package :cm.seed)

(in-package :cm.seed)

(export '(cm
          cm*
          cmlet))

;;; Tree walking and manipulation

(defun reduce-elements (n list new &optional append)
  "Replaces the first N elements in LIST with NEW. If APPEND is
  non-nil, append NEW to the list, otherwise cons."
  (if append
      (append new (nthcdr n list))
      (cons new (nthcdr n list))))

#+self-test.seed
(self-test.seed:define-self-test reduce-elements
  (equal (reduce-elements 1 '(1 2 3) '(a b)) '((a b) 2 3))
  (equal (reduce-elements 1 '(1 2 3) '(a b) t) '(a b 2 3))
  (equal (reduce-elements 2 '(1 2 3) '(a b)) '((a b) 3)))

;;; ^ foo => (return-from done foo)

(defun substitute-return (tree)
  (destructuring-bind (&optional return (value nil valuep)
                       &rest rest)
      tree
    (declare (ignore rest))
    (if (and (symbolp return)
             (string= (string return) "^")
             valuep)
        (reduce-elements 2 tree `(return-from done ,value))
        tree)))

#+self-test.seed
(self-test.seed:define-self-test substitute-return
  (equal '(a b c) (substitute-return '(a b c)))
  (equal '((return-from done nil)) (substitute-return '(^ nil))))

(defmacro with-caret-return (&body body)
  `(block done
     ,@(tree-walking.seed:map-tree-conses-postorder
        'substitute-return body)))

#+self-test.seed
(self-test.seed:define-self-test with-caret-return
  (equal 1 (with-caret-return
             ^ 1
             2)))

;;; a <- b         => (setf a b)
;;; (foo a) <- b   => (setf (foo a) b)
;;; a <- b <- c    => (setf a (setf b c))

(defun substitute-assignment (tree)
  (destructuring-bind (&optional place assignment? (value nil valuep)
                       &rest rest)
      tree
    (declare (ignore rest))
    (if (and (symbolp assignment?)
             (string= (string assignment?) "<-")
             valuep)
        (reduce-elements 3 tree `(setf ,place ,value))
        tree)))

#+self-test.seed
(self-test.seed:define-self-test substitute-assignment
  (equal '(a b c) (substitute-assignment '(a b c)))
  (equal '((setf a nil) x) (substitute-assignment '(a <- nil x))))

(defmacro with-arrow-assignment (&body body)
  `(progn ,@(tree-walking.seed:map-tree-conses-postorder
             'substitute-assignment body)))

#+self-test.seed
(self-test.seed:define-self-test with-arrow-assignment
  (equal 2 (with-arrow-assignment
             (let (a b)
               a <- b <- 1
               (+ a b)))))

;;; Combines with-arrow-assignment and with-caret-return.

(defmacro Cm (&rest body)
  `(with-arrow-assignment
     (with-caret-return
       ,@body)))

#+self-test.seed
(self-test.seed:define-self-test cm
  (equal (let (a b c d e)
           (flet (((setf foo) (new)
                    (setf e new)))
             (cm a <- nil
                 b <- 1
                 c <- d <- 2
                 (foo) <- 3
                 ^ (list a b c d e)
                 10)))
         (list nil 1 2 2 3)))

;;; Combines cm with let.

(defmacro Cmlet (bindings &body body)
  `(let ,bindings
     (cm ,@body)))

#+self-test.seed
(self-test.seed:define-self-test cmlet
  (equal '(1 2 3)
         (cmlet (a b (c 3))
           a <- 1
           b <- 2
           (list a b c))))

;;; Cm*

(defun match-binop (binop tree)
  (destructuring-bind (&optional a binop? (b nil b?) &rest rest) tree
    (declare (ignore rest))
    (when (and (symbolp binop?)
               (string= (string binop) (string binop?))
               b?)
      (list :left a
            :right b))))

#+self-test.seed
(self-test.seed:define-self-test match-binop
  (null (match-binop "<-" '(a <-)))
  (equal (list :left 'a :right 'b)
         (match-binop "<-" '(a <- b))))

(defun collect-assignment-variables (body)
  (let (variables)
    (tree-walking.seed:walk-tree-conses-preorder
     (lambda (tree)
       (alexandria:when-let ((match (match-binop "<-" tree)))
         (let ((left (getf match :left)))
           (when (symbolp left)
             (pushnew left variables)))))
     body)
    (nreverse variables)))

#+self-test.seed
(self-test.seed:define-self-test collect-assignment-variables
  (equal '(a b c)
         (collect-assignment-variables '(a <- b <- 1
                                         (foo a) <- 1
                                         (when foo
                                           c <- 3)))))

;;; Like cm, but automatically generates bindings.

(defmacro Cm* (&rest body)
  (let ((variables (collect-assignment-variables body)))
    `(let ,variables
       (cm ,@body))))

#+self-test.seed
(self-test.seed:define-self-test cm*
  (equal (list 1 2)
         (cm* a <- 1
              b <- 2
              (list a b))))
