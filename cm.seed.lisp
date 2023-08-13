(package.seed:define-seed-package :cm.seed)

(in-package :cm.seed)

(export '(cm
          cm+
          cma+
          cmlet))

;;; Tree walking and manipulation

(defun reduce-elements (n list new &optional append)
  "Replaces the first N elements in LIST with NEW. If APPEND is
  non-nil, append NEW to the list, otherwise cons. Does not check the
  length of LIST."
  (if append
      (append new (nthcdr n list))
      (cons new (nthcdr n list))))

#+self-test.seed
(self-test.seed:define-self-test reduce-elements
  (equal (reduce-elements 1 '(1 2 3) '(a b)) '((a b) 2 3))
  (equal (reduce-elements 1 '(1 2 3) '(a b) t) '(a b 2 3))
  (equal (reduce-elements 2 '(1 2 3) '(a b)) '((a b) 3)))

;;; Return operator

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

;;; Simple assignment

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
               (+ a b))))
  (equal 1 (with-arrow-assignment
             (let (a)
               (flet (((setf set-a) (new)
                        (setf a new)))
                 (set-a) <- 1
                 a)))))

;;; Destructuring assignment

;;; (&key a b) <- (list :a 1 :b 2)
;;;   => (destructuring-bind (&key a b) (list :a 1 :b 2) ...)
;;; (&optional a b) <- (list 1 2)
;;;   => (destructuring-bind (&optional a b) (list 1 2) ...)
;;; (x &key a) <- (list 1 :a 2)
;;;   => (destructuring-bind (x &key a) (list 1 :a 2) ...)

(defun looks-like-destructuring-form-p (form)
  (and (consp form)
       (loop :for object :in form
             :thereis (member object '(&optional &key &rest &aux &allow-other-keys)))))

(defun substitute-destructuring-assignment (tree)
  (destructuring-bind (&optional destructuring-form
                                 assignment?
                                 (value nil valuep)
                       &rest rest)
      tree
    (cond ((and (symbolp assignment?)
                (string= assignment? "<-")
                valuep
                (looks-like-destructuring-form-p destructuring-form))
           `((destructuring-bind ,destructuring-form ,value
              ,@rest)))
          (t tree))))

#+self-test.seed
(self-test.seed:define-self-test substitute-destructuring-assignment
  (equal '((destructuring-bind (&key a) (list :a 1)))
         (substitute-destructuring-assignment '((&key a) <- (list :a 1))))
  (equal '(a <- (list :a 1))
         (substitute-destructuring-assignment '(a <- (list :a 1)))))

(defmacro with-destructuring-assignment (&body body)
  `(progn ,@(tree-walking.seed:map-tree-conses-postorder
             'substitute-destructuring-assignment body)))

#+self-test.seed
(self-test.seed:define-self-test with-destructuring-assignment
  (equal '(1 2)
         (with-destructuring-assignment
           (&key a) <- (list :a 1)
           (&key b) <- (list :b 2)
           (list a b)))
  (equal '(1 2)
         (with-destructuring-assignment
           (&key a b) <- (list :a 1 :b 2)
           (when (and a b)
             (x y &key) <- (list a b)
             (list x y))))
  (eq :error
      ;; Muffle undefined variable warnings.
      (let ((f (handler-bind ((warning #'muffle-warning))
                 (compile nil '(lambda ()
                                (with-destructuring-assignment
                                  (&key a b) <- (list :a 1 :b 2)
                                  (when (and a b)
                                    (x y &key) <- (list a b))
                                  ;; Undefined.
                                  (list x y)))))))
        (handler-case (funcall f)
          (error () :error)))))

;;; Putting it all together.

;;; Combines with-arrow-assignment and with-caret-return.

(defmacro cm (&rest body)
  `(with-destructuring-assignment
     (with-arrow-assignment
       (with-caret-return
         ,@body))))

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
         (list nil 1 2 2 3))
  (equal (let (a)
           (cm a <- (list 1 :a 2 :b 2)
               (x &rest rest &key a &allow-other-keys) <- a
               (list x a rest)))
         '(1 2 (:a 2 :b 2))))

;;; Combines cm with let.

(defmacro cmlet (bindings &body body)
  `(let ,bindings
     (cm ,@body)))

#+self-test.seed
(self-test.seed:define-self-test cmlet
  (equal '(1 2 3)
         (cmlet (a b (c 3))
           a <- 1
           b <- 2
           (list a b c))))

;;; Cm+

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

(defmacro cm+ (&rest body)
  (let ((variables (collect-assignment-variables body)))
    `(let ,variables
       (cm ,@body))))

#+self-test.seed
(self-test.seed:define-self-test cm+
  (equal (list 1 2)
         (cm+ a <- 1
              b <- 2
              (list a b))))

;;; Cma+

;;; Provides a top-level "ANS" variable (interned into *package*) that
;;; contains the result of the last non-cm expression.

(defun skip-cm-expression (tree &optional acc)
  (flet ((symbol-string= (string thing)
           (and (symbolp thing)
                (string= string thing))))
    (destructuring-bind (&optional place arrow (value nil valuep)
                         &rest rest)
        tree
      (cond ((and valuep (symbol-string= "<-" arrow))
             (if (symbol-string= "<-" (car rest)) ; chain
                 (skip-cm-expression (cons value rest)
                                     (append acc (list place arrow)))
                 (skip-cm-expression rest
                                     (append acc (list place arrow value)))))
            ((symbol-string= "^" (car tree))
             (skip-cm-expression (cddr tree)
                                 (append acc (list (car tree) (cadr tree)))))
            (t (values tree acc))))))

#+self-test.seed
(self-test.seed:define-self-test skip-cm-expression
  (equal '(1) (skip-cm-expression '(1)))
  (equal '(2) (skip-cm-expression '(^ 1 2)))
  (equal '(2) (skip-cm-expression '(a <- 1 2)))
  (equal '(2) (skip-cm-expression '(a <- b <- 1 2))))

(defmacro cma+ (&rest body)
  `(cm+ ,@(loop :for (forms cm-expression) = (multiple-value-list
                                              (skip-cm-expression body))
                :then (multiple-value-list (skip-cm-expression (cdr forms)))
                :for head = (car forms)
                :while (or forms cm-expression)
                :when cm-expression
                :append cm-expression
                :when forms
                :append `(,(intern "ANS" *package*) <- ,head))))

#+self-test.seed
(self-test.seed:define-self-test cma+
  (equal (cma+ 1 ans)
         1)
  (equal (cma+ 1
               a <- (* 2 ans)
               ans)
         1)
  (equal (cma+ 1
               ^ ans
               2)
         1)
  (equal (cma+ 1
               (+ ans 1)
               a <- (* ans 0)
               (* ans 2))
         4))
