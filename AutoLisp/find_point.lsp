(defun c:PrintEntitiesInWindow ( / p1 p2 ss i entType startPt endPt pointsList)
  ;; ù ��° �� ����
  (setq p1 (getpoint "\nù ��° ���� �����ϼ���: "))
  ;; �� ��° �� ����
  (setq p2 (getcorner p1 "\n�ݴ��� ���� �����ϼ���: "))

  ;; ���õ� ���� ���� ��ü�� ����
  (setq ss (ssget "C" p1 p2))

  ;; ���õ� ��ü�� �ִ��� Ȯ��
  (if ss
    (progn
      (setq i 0)
      (setq pointsList '())  ;; ���۰� ������ ������ ����Ʈ �ʱ�ȭ
      ;; ���õ� ��ü���� ��ȸ�ϸ� ���� ���
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq entType (cdr (assoc 0 (entget ent))))
        
        ;; ��ü�� ���۰� ���� ����
        (cond
          ;; LINE ��ü ó��
          ((equal entType "LINE")
           (setq startPt (cdr (assoc 10 (entget ent))))
           (setq endPt (cdr (assoc 11 (entget ent))))
           (setq pointsList (cons (list startPt endPt) pointsList)))

          ;; POLYLINE ��ü ó��
          ((equal entType "LWPOLYLINE")
           (setq startPt (cdr (assoc 10 (entget ent))))
           ;; POLYLINE�� ������ ���� ����Ʈ�� ���� ��ġ
           (setq endPt (cdr (assoc 10 (last (entget ent)))))
           (setq pointsList (cons (list startPt endPt) pointsList)))
        )
        (setq i (1+ i)))

      ;; ���ε� ���
      (princ "\n���� ����Ʈ:")
      (mapcar
        '(lambda (pts)
           (princ (strcat "\n������: "
                          (rtos (car (car pts)) 2 2) ", "
                          (rtos (cadr (car pts)) 2 2) ", "
                          (rtos (caddr (car pts)) 2 2)
                          " - ����: "
                          (rtos (car (cadr pts)) 2 2) ", "
                          (rtos (cadr (cadr pts)) 2 2) ", "
                          (rtos (caddr (cadr pts)) 2 2))))
        pointsList)

      ;; ���Ἲ Ȯ�� �� ���� �Ǻ�
      (if (check-connected pointsList)
        (if (check-rectangle pointsList)
          (princ "\n����� ������ �簢���Դϴ�.")
          (if (check-circle pointsList)
            (princ "\n����� ������ ���Դϴ�.")
            (princ "\n����� ������ �簢���� ���� �ƴմϴ�.")))
        (princ "\n������ ����Ǿ� ���� �ʽ��ϴ�."))
    )
    (princ "\n���õ� ��ü�� �����ϴ�.")
  )

  (princ)
)

(defun check-connected (pointsList / connected)
  ;; ������ ����Ǿ� �ִ��� Ȯ���ϴ� �Լ�
  ;; ������ ���Ἲ �˻�: �� ������ �ٸ� �������� ��ġ�ϴ��� Ȯ��
  (setq connected t)
  (setq i 0)
  (while (and connected (< i (length pointsList)))
    (setq startPt (car (nth i pointsList)))
    (setq endPt (cadr (nth i pointsList)))
    (setq j 0)
    (setq match nil)
    (while (and (not match) (< j (length pointsList)))
      (if (and (equal endPt (car (nth j pointsList))) (not (equal i j)))
        (setq match t))
      (setq j (1+ j)))
    (if (not match)
      (setq connected nil))
    (setq i (1+ i)))
  connected)

(defun check-rectangle (pointsList)
  ;; ������ �簢�� �Ǻ� �Լ�: �� ���� ������ �ְ�, �� ���� ������ �������� Ȯ��
  (and (= (length pointsList) 4)
       (connected-points-rectangle pointsList)))

(defun connected-points-rectangle (pointsList / p1 p2 p3 p4)
  ;; �� ���� �������� ���簢���� �̷���� Ȯ��
  (setq p1 (car (car pointsList)))
  (setq p2 (cadr (car pointsList)))
  (setq p3 (car (cadr pointsList)))
  (setq p4 (cadr (cadr pointsList)))
  (setq p5 (car (caddr pointsList)))
  (setq p6 (cadr (caddr pointsList)))
  (setq p7 (car (cadddr pointsList)))
  (setq p8 (cadr (cadddr pointsList)))
  
  ;; ������ ������ ���簢���� �̷���� Ȯ��
  (and (equal p1 p8)
       (equal p2 p3)
       (equal p4 p5)
       (equal p6 p7)
       (perpendicular p1 p2 p3)
       (perpendicular p3 p4 p5)
       (perpendicular p5 p6 p7)
       (perpendicular p7 p8 p1)))

(defun perpendicular (pt1 pt2 pt3)
  ;; �־��� �� ���� ������ �̷���� Ȯ��
  (setq dx1 (- (car pt2) (car pt1)))
  (setq dy1 (- (cadr pt2) (cadr pt1)))
  (setq dx2 (- (car pt3) (car pt2)))
  (setq dy2 (- (cadr pt3) (cadr pt2)))
  (zerop (+ (* dx1 dx2) (* dy1 dy2))))

(defun check-circle (pointsList)
  ;; ������ �� �Ǻ� �Լ�: �߽������� ������ �Ÿ��� ��ġ�� �� ���� ���� �ִ��� Ȯ��
  ;; ������ �Ǻ��ϴ� ��ü���� ���� �߰�
  nil)  ;; �ӽ÷� �������� ��ȯ
