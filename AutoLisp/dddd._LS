(defun c:DrawIntersectingLines (/ p1 p2 gap total_length ent1 ent2 line1_pts line2_pts pts min_len i intersect_points)
  ;; 입력받기
  (setq p1 (getpoint "시작 위치를 선택하세요: "))
  (setq ent1 (car (entsel "첫 번째 라인 또는 폴리라인을 선택하세요: ")))
  (setq ent2 (car (entsel "두 번째 라인 또는 폴리라인을 선택하세요: ")))
  (setq gap (getdist "간격을 입력하세요: "))
  (setq total_length (getdist "총 길이를 입력하세요: "))

  ;; 두 엔티티의 포인트들을 구함
  (setq line1_pts (vlax-curve-getcoords ent1))
  (setq line2_pts (vlax-curve-getcoords ent2))

  ;; 두 라인의 길이 중 작은 것을 기준으로 함
  (setq min_len (min (vlax-curve-getdistatparam ent1 (vlax-curve-getendparam ent1))
                     (vlax-curve-getdistatparam ent2 (vlax-curve-getendparam ent2))))

  ;; 간격에 따라 점 생성
  (setq pts (list))
  (setq intersect_points (list))
  (setq i 0)
  (while (<= (* i gap) min_len)
    (setq x (+ (car p1) (* i gap)))
    (setq pts (cons (list x (cadr p1) (caddr p1)) pts))
    (setq i (1+ i))
  )

  ;; 교차점 찾기 및 라인 그리기
  (foreach pt pts
    (setq intersect1 (vlax-curve-getpointatdist ent1 (vlax-curve-getdistatpoint ent1 pt)))
    (setq intersect2 (vlax-curve-getpointatdist ent2 (vlax-curve-getdistatpoint ent2 pt)))
    (if (and intersect1 intersect2)
      (setq intersect_points (append intersect_points (list intersect1 intersect2)))
    )
  )

  ;; 교차점 연결
  (if intersect_points
    (entmake
      (append
        (list (cons 0 "POLYLINE") (cons 8 "0") (cons 100 "AcDbEntity") (cons 100 "AcDbPolyline"))
        (mapcar (lambda (pt) (list (cons 10 pt))) intersect_points)
      )
    )
  )

  ;; center 속성 추가
  (entmod (list (cons 100 "AcDbPolyline") (cons 70 1)))
  (princ)
)

(defun min (a b)
  (if (< a b) a b)
)
