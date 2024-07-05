(defun c:ShowProfile ( / ss entData uniqueId dxf0 dxf40 dxf100 sx sy ex ey cx cy radius startAngle endAngle midAngle pointList ent file centerPoint)
  (setq ss (ssget))  ; ����ڰ� ������ ��ü���� ���� �������� ��ȯ
  (if ss
    (progn
      ; ����ڰ� �߽����� �����ϵ��� ��
      (setq centerPoint (getpoint "\nSelect center point: "))

      (setq file (open "C:\\Users\\admin\\Desktop\\Tekla_Project\\AutoLisp\\data_center.csv" "w"))  ; CSV ���� ��� ���� �� ���� ����
      (write-line "Entity ID,Type,DXF40,Model,Points" file)  ; CSV ��� �ۼ�
      (setq uniqueId 0)  ; ���� ��ȣ �ʱ�ȭ
      (repeat (sslength ss)  ; ���� ������ ũ�⸸ŭ �ݺ�
        (setq ent (ssname ss uniqueId))  ; uniqueId �ε����� ��ü�� �ڵ��� ����
        (setq entData (entget ent))  ; ��ƼƼ �����͸� ������
        (setq dxf0 (cdr (assoc 0 entData)))
        (setq dxf40 (cdr (assoc 40 entData)))
        (setq dxf40 (if dxf40 (rtos dxf40 2 2) "0.0"))  ; DXF 40�� ������ 0.0���� ����
        (setq dxf100 (cdr (assoc 100 entData)))
        (setq pointList '())  ; ����Ʈ ������ ����Ʈ �ʱ�ȭ

        (foreach pt (vl-remove-if-not '(lambda (x) (= (car x) 10)) entData)
          (setq x (nth 0 (cdr pt)))
          (setq y (nth 1 (cdr pt)))
          (setq pointList (cons (list (rtos x 2 2) (rtos y 2 2)) pointList))
        )
        
        (if (assoc 11 entData)
          (foreach pt (vl-remove-if-not '(lambda (x) (= (car x) 11)) entData)
            (setq x (nth 0 (cdr pt)))
            (setq y (nth 1 (cdr pt)))
            (setq pointList (cons (list (rtos x 2 2) (rtos y 2 2)) pointList))
          )
        )

        (cond
          ((= dxf0 "ARC")  ; ��ƼƼ Ÿ���� ARC�� ���
            (setq cx (nth 0 (cdr (assoc 10 entData))))  ; �߽��� X ��ǥ
            (setq cy (nth 1 (cdr (assoc 10 entData))))  ; �߽��� Y ��ǥ
            (setq radius (cdr (assoc 40 entData)))  ; ������
            (setq startAngle (* (cdr (assoc 50 entData)) (/ pi 180.0)))  ; ���۰� (�������� ��ȯ)
            (setq endAngle (* (cdr (assoc 51 entData)) (/ pi 180.0)))  ; ���� (�������� ��ȯ)
            (setq midAngle (/ (+ startAngle endAngle) 2.0))  ; �߰���
            (setq sx (+ cx (* radius (cos startAngle))))  ; ������ X ��ǥ
            (setq sy (+ cy (* radius (sin startAngle))))  ; ������ Y ��ǥ
            (setq ex (+ cx (* radius (cos endAngle))))  ; ���� X ��ǥ
            (setq ey (+ cy (* radius (sin endAngle))))  ; ���� Y ��ǥ
            (setq mx (+ cx (* radius (cos midAngle))))  ; �߰��� X ��ǥ
            (setq my (+ cy (* radius (sin midAngle))))  ; �߰��� Y ��ǥ
            (setq pointList (list
                              (list (rtos sx 2 2) (rtos sy 2 2))
                              (list (rtos mx 2 2) (rtos my 2 2))
                              (list (rtos ex 2 2) (rtos ey 2 2))
                              (list (rtos cx 2 2) (rtos cy 2 2)) ; �߽����� ����Ʈ ����Ʈ�� �߰�
                            ))  ; ����Ʈ ����Ʈ�� ������, �߰���, ���� �߰�
          )
          ((or (= dxf0 "CIRCLE") (= dxf0 "POINT") (= dxf0 "LINE") (= dxf0 "POLYLINE"))  ; �ٸ� ��ƼƼ Ÿ�� ó��
            (setq cx (nth 0 (cdr (assoc 10 entData))))  ; �߽��� X ��ǥ
            (setq cy (nth 1 (cdr (assoc 10 entData))))  ; �߽��� Y ��ǥ
            (setq pointList (cons (list (rtos cx 2 2) (rtos cy 2 2)) pointList)) ; �߽����� ����Ʈ ����Ʈ�� �߰�
          )
        )

        ; CSV ���Ͽ� ������ ����
        (write-line 
          (strcat 
            (itoa uniqueId) "," 
            dxf0 "," 
            dxf40 "," 
            dxf100 ",\"" 
            (apply 'strcat (mapcar '(lambda (pt) (strcat "(" (car pt) ", " (cadr pt) ") ")) pointList)) 
            "\""
          ) 
          file
        )

        (setq uniqueId (+ uniqueId 1))  ; ���� ��ȣ ����
      )

      ; �߽��� ������ CSV�� �߰�
      (setq cx (rtos (car centerPoint) 2 2))
      (setq cy (rtos (cadr centerPoint) 2 2))
      (write-line 
        (strcat 
          "-1,center,0.0,,\"" 
          "(" cx ", " cy ")\""
        ) 
        file
      )

      (close file)  ; ���� �ݱ�
      (princ (strcat "\nCenter Point: (" cx ", " cy ")"))  ; ���õ� �߽��� ���
      (princ "\nData has been exported to CSV successfully.")
    )
    (prompt "\nNo objects selected.")  ; ��ü ���õ��� �ʾ��� ��� �޽��� ���
  )
  (princ)  ; �Լ� ����
)
