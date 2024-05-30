(defun c:PrintEntitiesInWindow ( / p1 p2 ss i entType startPt endPt pointsList)
  ;; 첫 번째 점 선택
  (setq p1 (getpoint "\n첫 번째 점을 선택하세요: "))
  ;; 두 번째 점 선택
  (setq p2 (getcorner p1 "\n반대쪽 점을 선택하세요: "))

  ;; 선택된 범위 내의 객체들 선택
  (setq ss (ssget "C" p1 p2))

  ;; 선택된 객체가 있는지 확인
  (if ss
    (progn
      (setq i 0)
      (setq pointsList '())  ;; 시작과 끝점을 저장할 리스트 초기화
      ;; 선택된 객체들을 순회하며 정보 출력
      (while (< i (sslength ss))
        (setq ent (ssname ss i))
        (setq entType (cdr (assoc 0 (entget ent))))
        
        ;; 객체의 시작과 끝점 추출
        (cond
          ;; LINE 객체 처리
          ((equal entType "LINE")
           (setq startPt (cdr (assoc 10 (entget ent))))
           (setq endPt (cdr (assoc 11 (entget ent))))
           (setq pointsList (cons (list startPt endPt) pointsList)))

          ;; POLYLINE 객체 처리
          ((equal entType "LWPOLYLINE")
           (setq startPt (cdr (assoc 10 (entget ent))))
           ;; POLYLINE의 마지막 점은 리스트의 끝에 위치
           (setq endPt (cdr (assoc 10 (last (entget ent)))))
           (setq pointsList (cons (list startPt endPt) pointsList)))
        )
        (setq i (1+ i)))

      ;; 라인들 출력
      (princ "\n라인 리스트:")
      (mapcar
        '(lambda (pts)
           (princ (strcat "\n시작점: "
                          (rtos (car (car pts)) 2 2) ", "
                          (rtos (cadr (car pts)) 2 2) ", "
                          (rtos (caddr (car pts)) 2 2)
                          " - 끝점: "
                          (rtos (car (cadr pts)) 2 2) ", "
                          (rtos (cadr (cadr pts)) 2 2) ", "
                          (rtos (caddr (cadr pts)) 2 2))))
        pointsList)

      ;; 연결성 확인 및 도형 판별
      (if (check-connected pointsList)
        (if (check-rectangle pointsList)
          (princ "\n연결된 도형은 사각형입니다.")
          (if (check-circle pointsList)
            (princ "\n연결된 도형은 원입니다.")
            (princ "\n연결된 도형은 사각형도 원도 아닙니다.")))
        (princ "\n노드들이 연결되어 있지 않습니다."))
    )
    (princ "\n선택된 객체가 없습니다.")
  )

  (princ)
)

(defun check-connected (pointsList / connected)
  ;; 노드들이 연결되어 있는지 확인하는 함수
  ;; 간단한 연결성 검사: 각 끝점이 다른 시작점과 일치하는지 확인
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
  ;; 간단한 사각형 판별 함수: 네 개의 선분이 있고, 네 개의 끝점이 만나는지 확인
  (and (= (length pointsList) 4)
       (connected-points-rectangle pointsList)))

(defun connected-points-rectangle (pointsList / p1 p2 p3 p4)
  ;; 네 개의 꼭짓점이 직사각형을 이루는지 확인
  (setq p1 (car (car pointsList)))
  (setq p2 (cadr (car pointsList)))
  (setq p3 (car (cadr pointsList)))
  (setq p4 (cadr (cadr pointsList)))
  (setq p5 (car (caddr pointsList)))
  (setq p6 (cadr (caddr pointsList)))
  (setq p7 (car (cadddr pointsList)))
  (setq p8 (cadr (cadddr pointsList)))
  
  ;; 점들의 순서가 직사각형을 이루는지 확인
  (and (equal p1 p8)
       (equal p2 p3)
       (equal p4 p5)
       (equal p6 p7)
       (perpendicular p1 p2 p3)
       (perpendicular p3 p4 p5)
       (perpendicular p5 p6 p7)
       (perpendicular p7 p8 p1)))

(defun perpendicular (pt1 pt2 pt3)
  ;; 주어진 세 점이 직각을 이루는지 확인
  (setq dx1 (- (car pt2) (car pt1)))
  (setq dy1 (- (cadr pt2) (cadr pt1)))
  (setq dx2 (- (car pt3) (car pt2)))
  (setq dy2 (- (cadr pt3) (cadr pt2)))
  (zerop (+ (* dx1 dx2) (* dy1 dy2))))

(defun check-circle (pointsList)
  ;; 간단한 원 판별 함수: 중심점에서 동일한 거리에 위치한 네 개의 점이 있는지 확인
  ;; 원인지 판별하는 구체적인 로직 추가
  nil)  ;; 임시로 거짓으로 반환
