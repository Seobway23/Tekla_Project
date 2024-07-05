(defun c:CCCC (/ p1 p2 line1 line2 spacing length pts1 pts2 i pt1 pt2 newLine)
  ;; �Է� �ޱ�
  (setq p1 (getpoint "\nù ���� ��ġ�� �����ϼ���: "))
  (setq line1 (entsel "\nù ��° ������ �����ϼ���: "))
  (setq line2 (entsel "\n�� ��° ������ �����ϼ���: "))
  (setq spacing (getdist "\n������ �����ϼ���: "))
  (setq length (getdist "\n�� ���̸� �����ϼ���: "))

  ;; ��ƼƼ ���
  (setq line1 (car line1))
  (setq line2 (car line2))

  ;; ������ �� ��� ���
  (setq pts1 (vlax-curve-getPoints line1 0.0 1.0))
  (setq pts2 (vlax-curve-getPoints line2 0.0 1.0))

  ;; �� ���� �� ���� ������ ���̸� �������� ����
  (if (> (length pts1) (length pts2))
    (setq pts1 (vlax-curve-getPoints line2 0.0 1.0)
          pts2 (vlax-curve-getPoints line1 0.0 1.0))
  )

  ;; ������ ���ݿ� �°� �����ϰ� ������ ����
  (setq i 0)
  (while (and (< (* i spacing) length) (< i (length pts1)))
    (setq pt1 (nth i pts1))
    (setq pt2 (nth i pts2))

    ;; ���� �����ϴ� �� ����
    (setq newLine (entmake
      (list
        (cons 0 "LINE")
        (cons 10 pt1)
        (cons 11 pt2)
        (cons 62 1)  ;; ���� ���� (���� ����)
        (cons 8 "center")  ;; ���̾� ���� (���� ����)
      )
    ))
    (setq i (1+ i))
  )
  (princ)
)

(defun vlax-curve-getPoints (ent start end)
  (let ((param (vlax-curve-getParamAtPoint ent start))
        (endParam (vlax-curve-getParamAtPoint ent end))
        (pts '())
        (step (/ (- endParam param) 100.0)))
    (while (<= param endParam)
      (setq pts (append pts (list (vlax-curve-getPointAtParam ent param))))
      (setq param (+ param step))
    )
    pts
  )
)
