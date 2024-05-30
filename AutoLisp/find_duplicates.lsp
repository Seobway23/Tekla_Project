(defun mapcar-stop (func lst / result stop)
  (setq result '())
  (setq stop nil)
  (while (and lst (not stop))
    (setq result (cons (apply func (list (car lst))) result))
    (if (apply func (list (car lst)))
      (setq stop t))
    (setq lst (cdr lst)))
  (reverse result))

(defun find-duplicate-line (line1 lineList)
  (mapcar-stop '(lambda (line2)
                  (and (not (eq line1 line2))
                       (equal (vlax-get line1 'StartPoint) (vlax-get line2 'StartPoint) 0.0001)
                       (equal (vlax-get line1 'EndPoint) (vlax-get line2 'EndPoint) 0.0001)))
                lineList))

(defun c:FindDuplicateLines (/ ss lineList duplicates)
  (vl-load-com)
  (setq ss (ssget "L"))
  (if ss
    (progn
      (setq lineList (mapcar 'vlax-ename->vla-object (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss)))))
      (setq duplicates '())
      (foreach line1 lineList
        (if (find-duplicate-line line1 lineList)
          (setq duplicates (cons (vlax-get line1 'Handle) duplicates))))
      (if duplicates
        (progn
          (princ "\nDuplicate Lines Found:")
          (foreach line duplicates
            (princ (strcat "\nDuplicate Line: " line)))
          (princ "\nDuplicate lines found and listed."))
        (princ "\nNo duplicate lines found.")))
    (princ "\nNo lines selected."))
  (princ))
