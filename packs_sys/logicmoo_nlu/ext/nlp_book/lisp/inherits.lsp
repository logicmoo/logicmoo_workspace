;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;; %    Example code from the book "Natural Language Processing in LISP"   %
;;; %                      published by Addison Wesley                      %
;;; %        Copyright (c) 1989, Gerald Gazdar & Christopher Mellish.       %
;;; % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
;;;
;;; inherits.lsp [Chapter  9] simple semantic net with default inheritance
                     
(defun attr (entity attribute value)
  (setf (get entity attribute) value))

(defun isa (entity1 entity2)
  (setf (get entity1 'isa)
    (cons entity2 (get entity1 'isa))))

(attr 'club_member 'sex 'male)
(attr 'club_member 'over_50 'yes)
(attr 'club_member 'citizenship 'US)

(isa 'associate 'club_member)
(attr 'associate 'associate_member 'yes)
(attr 'associate 'citizenship 'non_US)

(isa 'life_member 'club_member)
(attr 'life_member 'life_member 'yes)
(attr 'life_member 'over_50 'no)

(isa 'kim 'associate)
(attr 'kim 'over_50 'no)

(isa 'jean 'associate)
(attr 'jean 'sex 'female)
(attr 'jean 'citizenship 'US)

(isa 'mayumi 'life_member)
(attr 'mayumi 'sex 'female)
(attr 'mayumi 'over_50 'yes)
(attr 'mayumi 'citizenship 'non_US)

(isa 'beryl 'life_member)
(attr 'beryl 'sex 'female)

(defun get_attr (entity attribute)
  (catch 'got_one
    (if (get entity attribute)
      (throw 'got_one (get entity attribute))
      (dolist (e1 (get entity 'isa))
        (let ((x (get_attr e1 attribute)))
          (if x
            (throw 'got_one x)))))
    nil))
