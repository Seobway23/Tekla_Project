(defun get-intersection-points (polyline xInterval startX)
  (setq intersectionPoints '())

  ;; Iterate over the vertices of the polyline
  (setq vertList (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget polyline))))
  (setq numVerts (length vertList))

  (setq i 0)
  (while (< i (- numVerts 1))
    (setq pt1 (nth i vertList))
    (setq pt2 (nth (1+ i) vertList))
    (setq x1 (car pt1) y1 (cadr pt1))
    (setq x2 (car pt2) y2 (cadr pt2))

    ;; Check if the segment crosses any xInterval points starting from startX
    (setq j 0)
    (while (<= (+ startX (* j xInterval)) (max x1 x2))
      (setq x (+ startX (* j xInterval)))
      (if (and (<= (min x1 x2) x (max x1 x2)) (/= x1 x2)) ;; Avoid division by zero
        (setq y (+ y1 (* (/ (- y2 y1) (- x2 x1)) (- x x1)))
              intersectionPoints (cons (list x y 0) intersectionPoints)))
      (setq j (1+ j))
      (if (> x (max x1 x2)) (setq j (+ j (fix (/ (- x (max x1 x2)) xInterval)))))) ; ���� ���� ���� �߰�

    (setq i (1+ i)))

  (reverse intersectionPoints))

(defun c:CreateLinesBetweenTwoLines (/ p1 line1 line2 spacing length pts1 pts2 pt1 pt2 i newLine)
  ;; �Է� �ޱ�
  (setq p1 (getpoint "\nù ���� ��ġ�� �����ϼ���: "))
  (setq line1 (car (entsel "\nù ��° ������ �����ϼ���: ")))
  (setq line2 (car (entsel "\n�� ��° ������ �����ϼ���: ")))
  (setq spacing (getdist "\n������ �����ϼ���: "))
  (setq length (getdist "\n�� ���̸� �����ϼ���: "))

  ;; ���� ��ġ�� X ��ǥ ���
  (setq startX (car p1))

  ;; ù ��° ���ο��� �� ���
  (setq pts1 (get-intersection-points line1 spacing startX))
  ;; �� ��° ���ο��� �� ���
  (setq pts2 (get-intersection-points line2 spacing startX))

  ;; �� ���� �� ���� ���� ������ �������� ����
  (setq minPts (min (length pts1) (length pts2)))

  ;; ������ ���ݿ� �°� �����ϰ� ������ ����
  (setq i 0)
  (while (< i minPts)
    (setq pt1 (nth i pts1))
    (setq pt2 (nth i pts2))

    ;; ���� �����ϴ� �� ����
    (setq newLine (entmake
      (list
        (cons 0 "LINE")
        (cons 10 pt1)
        (cons 11 pt2)
        (cons 6 "CENTER")  ;; �� ���� ����
      )
    ))
    (setq i (1+ i))
  )
  (princ)
)
