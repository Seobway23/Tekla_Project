(defun c:CCCC (/ p1 p2 line1 line2 spacing length pts1 pts2 i pt1 pt2 newLine)
  ;; 입력 받기
  (setq p1 (getpoint "\n첫 시작 위치를 지정하세요: "))
  (setq line1 (entsel "\n첫 번째 라인을 선택하세요: "))
  (setq line2 (entsel "\n두 번째 라인을 선택하세요: "))
  (setq spacing (getdist "\n간격을 설정하세요: "))
  (setq length (getdist "\n총 길이를 설정하세요: "))

  ;; 엔티티 얻기
  (setq line1 (car line1))
  (setq line2 (car line2))

  ;; 라인의 점 목록 얻기
  (setq pts1 (vlax-curve-getPoints line1 0.0 1.0))
  (setq pts2 (vlax-curve-getPoints line2 0.0 1.0))

  ;; 두 라인 중 작은 라인의 길이를 기준으로 설정
  (if (> (length pts1) (length pts2))
    (setq pts1 (vlax-curve-getPoints line2 0.0 1.0)
          pts2 (vlax-curve-getPoints line1 0.0 1.0))
  )

  ;; 점들을 간격에 맞게 생성하고 선으로 연결
  (setq i 0)
  (while (and (< (* i spacing) length) (< i (length pts1)))
    (setq pt1 (nth i pts1))
    (setq pt2 (nth i pts2))

    ;; 점들 연결하는 선 생성
    (setq newLine (entmake
      (list
        (cons 0 "LINE")
        (cons 10 pt1)
        (cons 11 pt2)
        (cons 62 1)  ;; 색상 설정 (선택 사항)
        (cons 8 "center")  ;; 레이어 설정 (선택 사항)
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
