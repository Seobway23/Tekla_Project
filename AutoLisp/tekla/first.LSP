(defun c:selectedentity ()

  (setq ss (ssget))
  (if ss
    (setq entss (entget ss))
    (princ entss)

   )


       )