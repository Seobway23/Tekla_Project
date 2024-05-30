(defun c:CleanUpDrawing ( / ss i j line1 line2 start1 end1 start2 end2 tolerance newLine)
  ;; 도면 내의 모든 라인을 선택
  (setq ss (ssget "X" '((0 . "LINE"))))
  (setq tolerance 0.01) ;; 허용 오차 설정

  ;; 선택된 라인이 있는지 확인
  (if ss
    (progn
      ;; 끊어진 라인 연결
      (setq i 0)
      (while (< i (sslength ss))
        (setq line1 (ssname ss i))
        (setq start1 (cdr (assoc 10 (entget line1))))
        (setq end1 (cdr (assoc 11 (entget line1))))
        (setq j (1+ i))
        (while (< j (sslength ss))
          (setq line2 (ssname ss j))
          (setq start2 (cdr (assoc 10 (entget line2))))
          (setq end2 (cdr (assoc 11 (entget line2))))
          ;; 라인의 끝점과 다른 라인의 시작점이 근접한 경우 연결
          (cond
            ;; 첫 번째 라인의 끝점(end1)과 두 번째 라인의 시작점(start2)이 근접한 경우
            ((< (distance end1 start2) tolerance)
             ;; 새로운 라인 생성
             (setq newLine (list (cons 0 "LINE")
                                 (cons 10 start1)
                                 (cons 11 end2)))
             (entmake newLine)
             ;; 기존 라인 삭제
             (entdel line1)
             (entdel line2)
             ;; 선택 집합 갱신 및 초기화
             (setq ss (ssget "X" '((0 . "LINE"))))
             (setq i -1) ;; 외부 while 루프 재시작
             (setq j (sslength ss))) ;; 내부 while 루프 종료
            ;; 첫 번째 라인의 시작점(start1)과 두 번째 라인의 끝점(end2)이 근접한 경우
            ((< (distance start1 end2) tolerance)
             ;; 새로운 라인 생성
             (setq newLine (list (cons 0 "LINE")
                                 (cons 10 end1)
                                 (cons 11 start2)))
             (entmake newLine)
             ;; 기존 라인 삭제
             (entdel line1)
             (entdel line2)
             ;; 선택 집합 갱신 및 초기화
             (setq ss (ssget "X" '((0 . "LINE"))))
             (setq i -1) ;; 외부 while 루프 재시작
             (setq j (sslength ss))) ;; 내부 while 루프 종료
            ;; 첫 번째 라인의 시작점(start1)과 두 번째 라인의 시작점(start2)이 근접한 경우
            ((< (distance start1 start2) tolerance)
             ;; 새로운 라인 생성
             (setq newLine (list (cons 0 "LINE")
                                 (cons 10 end1)
                                 (cons 11 end2)))
             (entmake newLine)
             ;; 기존 라인 삭제
             (entdel line1)
             (entdel line2)
             ;; 선택 집합 갱신 및 초기화
             (setq ss (ssget "X" '((0 . "LINE"))))
             (setq i -1) ;; 외부 while 루프 재시작
             (setq j (sslength ss))) ;; 내부 while 루프 종료
            ;; 첫 번째 라인의 끝점(end1)과 두 번째 라인의 끝점(end2)이 근접한 경우
            ((< (distance end1 end2) tolerance)
             ;; 새로운 라인 생성
             (setq newLine (list (cons 0 "LINE")
                                 (cons 10 start1)
                                 (cons 11 start2)))
             (entmake newLine)
             ;; 기존 라인 삭제
             (entdel line1)
             (entdel line2)
             ;; 선택 집합 갱신 및 초기화
             (setq ss (ssget "X" '((0 . "LINE"))))
             (setq i -1) ;; 외부 while 루프 재시작
             (setq j (sslength ss)))) ;; 내부 while 루프 종료
          (setq j (1+ j))))
        (setq i (1+ i)))

      ;; 겹치는 라인 병합
      (setq i 0)
      (while (< i (sslength ss))
        (setq line1 (ssname ss i))
        (setq start1 (cdr (assoc 10 (entget line1))))
        (setq end1 (cdr (assoc 11 (entget line1))))
        (setq j (1+ i))
        (while (< j (sslength ss))
          (setq line2 (ssname ss j))
          (setq start2 (cdr (assoc 10 (entget line2))))
          (setq end2 (cdr (assoc 11 (entget line2))))
          ;; 라인이 겹치는 경우 병합
          (if (or (and (equal start1 start2 tolerance)
                       (equal end1 end2 tolerance))
                  (and (equal start1 end2 tolerance)
                       (equal end1 start2 tolerance)))
            (progn
              ;; 기존 라인 삭제
              (entdel line2)
              ;; 선택 집합 갱신
              (setq ss (ssget "X" '((0 . "LINE"))))
              (setq j (sslength ss))) ;; 내부 while 문을 빠져나가도록 설정
          (setq j (1+ j))))
        (setq i (1+ i))))
    (princ "\n선택된 라인이 없습니다.")
  )

  (princ "\n도면 정리가 완료되었습니다.")
  (princ)
)

(defun distance (pt1 pt2)
  ;; 두 점 사이의 거리를 계산하는 함수
  (sqrt (+ (expt (- (car pt2) (car pt1)) 2)
           (expt (- (cadr pt2) (cadr pt1)) 2)
           (expt (- (caddr pt2) (caddr pt1)) 2))))
