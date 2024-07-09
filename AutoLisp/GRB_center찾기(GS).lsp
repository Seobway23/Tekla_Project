(defun c:GS (/ p1 p2 ss num i ent entData pt1 pt2 len totalLength numGRBCenterLines averageLength lineData tblObj tblRow tblPos rowHeight colWidth txtHeight)
  ;; ����ڰ� �巡���Ͽ� ������ �� �ֵ��� �� ���� ���� �Է¹���
  (setq p1 (getpoint "\nù ��° ���� �����Ͻʽÿ�: "))
  (setq p2 (getcorner p1 "\n�� ��° ���� �����Ͻʽÿ�: "))

  ;; ���� �������� GRB_CENTER ���̾ �ִ� ��� ������ ����
  (setq ss (ssget "C" p1 p2 '((8 . "GRB_CENTER") (0 . "LINE"))))
  
  (if ss
    (progn
      ;; �ʱ�ȭ
      (setq num (sslength ss))
      (setq i 0)
      (setq totalLength 0.0)
      (setq numGRBCenterLines 0)
      (setq lineData '())
      (setq rowHeight 400.0) ;; �� ���� ����
      (setq colWidth 2000.0) ;; �� �ʺ� ����
      (setq txtHeight 300) ;; �ؽ�Ʈ ���� ����

      ;; ���� ������ �� ���� ��ȸ�ϸ� ������ ���
      (while (< i num)
        (setq ent (ssname ss i))
        (setq entData (entget ent))
        (setq pt1 (cdr (assoc 10 entData)))
        (setq pt2 (cdr (assoc 11 entData)))
        (setq len (distance pt1 pt2))

        ;; GRB_CENTER ���̾ �ִ� ������ ������ ���
        (setq totalLength (+ totalLength len))
        (setq numGRBCenterLines (1+ numGRBCenterLines))

        ;; ���� �����͸� ����Ʈ�� �߰�
        (setq lineData (append lineData (list (list (1+ i) len))))

        (setq i (1+ i)))
      
      ;; ��� ���̸� ���
      (if (> numGRBCenterLines 0)
        (setq averageLength (/ totalLength numGRBCenterLines))
        (setq averageLength 0.0))

      ;; ǥ ��ġ ����
      (setq tblPos (getpoint "\nǥ ��ġ�� Ŭ���Ͻʽÿ�: "))

      ;; ���鿡 ǥ ����
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq modelSpace (vla-get-ModelSpace doc))
      (setq tblObj (vla-AddTable modelSpace (vlax-3d-point tblPos) (+ numGRBCenterLines 2) 2 rowHeight colWidth))

      ;; ǥ �ؽ�Ʈ ���� ����
      (vla-SetTextHeight tblObj (+ acDataRow acHeaderRow acTitleRow) txtHeight)

      ;; ǥ ��� ����
      (vla-SetText tblObj 0 0 "��ȣ")
      (vla-SetText tblObj 0 1 "����")

      ;; ǥ�� ������ �߰�
      (setq tblRow 1)
      (foreach line lineData
        (vla-SetText tblObj tblRow 0 (itoa (car line)))
        (vla-SetText tblObj tblRow 1 (rtos (cadr line) 2 2))
        (setq tblRow (1+ tblRow))
      )

      ;; �հ� ���� �߰�
      (vla-SetText tblObj tblRow 0 "�� ����")
      (vla-SetText tblObj tblRow 1 (rtos totalLength 2 2))
    )
    (princ "\n���õ� ������ GRB_CENTER ���̾ ���� �����ϴ�.")
  )

  (princ)
)

(princ "\nLISP ��ɾ �ε�Ǿ����ϴ�. 'GS' ��ɾ ����Ͽ� GRB_CENTER ���̾��� ������ �м��Ͻʽÿ�.")
(princ)






	    ;; ������ �࿡ "��ü ����" �߰�
	    (setq total-length 0.0)
	    (setq col 2)
	    (while (< col (1+ (length (car table_list))))
	      (setq total-length (+ total-length (rtos (sum-list (nth (1- col) table_list)) 2 2)))
	      (setq col (1+ col))
	    )
	    (vla-SetText tblObj (1+ row) 0 "��ü ����")
	    (vla-SetText tblObj (1+ row) col (rtos total-length 2 2))