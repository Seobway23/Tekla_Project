(defun c:FG (/ base-set intersect-set ent1 ent2 pts intersec-pts i j sp ep indi-center-pts)
  ;; ������ ��ǥ�� ������ ����Ʈ �ʱ�ȭ
  (setq intersec-pts '())
  (setq table_list '())

  ;; ���� �� ����Ʈ ����
  (princ "\n���� �� ����Ʈ�� �����ϼ���:")
  (setq base-set (ssget '((0 . "LWPOLYLINE,LINE") (8 . "GRB_CENTER"))))

  ;; �����Ǵ� �� ����Ʈ ����
  (princ "\n�����Ǵ� �� ����Ʈ�� �����ϼ���:")
  (setq intersect-set (ssget '((0 . "LWPOLYLINE,LINE"))))

  ;; �� ����Ʈ�� �����ϸ� ���� �۾��� ����
  (if (and base-set intersect-set)
    (progn
      (princ "\n���� �� ����Ʈ�� �����Ǵ� �� ����Ʈ�� �����߽��ϴ�.\n")
      ;; ���� �� ����Ʈ�� �����Ǵ� �� ����Ʈ�� �������� ã�´�.
      (setq i 0)
      (while (< i (sslength base-set))
	(setq dddd (sslength base-set))
	(princ "\n��ü����i:")
	(princ dddd)
        (setq ent1 (ssname base-set i))
        (setq indi-center-pts '())
	(setq indi-leng-pts '()) ;; indi-leng-pts �ʱ�ȭ
	
        (setq j 0)
        (while (< j (sslength intersect-set))
          (setq ent2 (ssname intersect-set j))
          (setq pts (acet-geom-intersection (vlax-ename->vla-object ent1)
                                            (vlax-ename->vla-object ent2)))
          (if pts
            (progn
              (setq indi-center-pts (append indi-center-pts (list pts)))
            )
            (princ "\n������ ����.")
          )
          (setq j (1+ j))
        )

        (princ (strcat "\nindi-points: " (vl-princ-to-string indi-center-pts)))

        ;; sp, ep ����
        (setq sp (cdr (assoc 10 (entget ent1))))
        (setq ep (cdr (assoc 11 (entget ent1))))

        ;; �߰�
        (setq indi-center-pts (append indi-center-pts (list sp)))
        (setq indi-center-pts (cons ep indi-center-pts))

        ;; ������ x ��ǥ�� �������� �������� ����
        (setq indi-center-pts (vl-sort indi-center-pts (function (lambda (a b) (> (car a) (car b))))))

        ;; �⺻ ����
	(setq intersec-pts (append intersec-pts indi-center-pts))
	(setq i (1+ i))

	;; ���� ����Ʈ�� length table �߰�
	(setq k 0) ;; �ε����� 0���� ����

	(princ "\n�ݺ�Ƚ��:")
	(princ (length indi-center-pts))

	(while (< k (- (length indi-center-pts) 1)) ;; k�� intersec-pts�� ���� - 1��ŭ �ݺ�
	  (setq pt1 (nth k indi-center-pts))      ;; k��° ���
	  (setq pt2 (nth (1+ k) indi-center-pts)) ;; k+1��° ���
	  (setq leng (- (cadr pt1) (cadr pt2))) ;; y��ǥ�� ���� ���
	  (setq indi-leng-pts (append indi-leng-pts (list leng)))
	  (setq k (1+ k))
	)
	(setq table_list (append table_list indi-leng-pts))
	
	
      )

  
      
      
      ;; ������ ��ǥ�� ����Ѵ�.
      (if intersec-pts
	(progn
        (princ (strcat "\n���� ������: " (vl-princ-to-string intersec-pts)))
	(princ "\nTable:")
	(princ table_list)
        )

      )
    )
    (princ "\n�� ���� �� ����Ʈ�� ��� �����ؾ� �մϴ�.")
  )	
  (princ)
)

(defun acet-geom-intersection (obj1 obj2 / int-pts)
  ;; �� ��ü�� �������� ã�� �Լ�
  (setq int-pts (vlax-invoke obj1 'IntersectWith obj2 acExtendNone))
  int-pts
)
