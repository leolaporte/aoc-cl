
(5a:test map-seed-test
  (5a:is (equal  ; above
          (map-seed '((30 50 10) (100 100 100))
                    '(55 10))
          '((35 5) (60 5))))

  (5a:is (equal  ; above
          (map-seed '((100 44 4) (200 300 200))
                    '(47 3))
          '((103 1) (48 2))))

  (5a:is (equal  ; below
          (map-seed '((0 100 10) (100 200 20))
                    '(199 5))
          '((100 4) (199 1))))

  (5a:is (equal  ;below
          (map-seed '((200 300 200) (0 102 100))
                    '(100 4))
          '((0 2) (100 2))))

  (5a:is (equal  ; src-contains
          (map-seed '((10 99 7) (200 300 200))
                    '(100 5))
          '((11 5))))

  (5a:is (equal  ; src-contains
          (map-seed  '((0 100 10) (100 200 20))
                     '(201 5))
          '((101 5))))

  (5a:is (equal  ; same
          (map-seed  '((0 100 10) (100 200 20))
                     '(200 20))
          '((100 20))))

  (5a:is (equal  ; same
          (map-seed '((100 50 10) (200 300 200))
                    '(50 10))
          '((100 10))))

  (5a:is (equal  ; seed-contains
          (map-seed  '((0 100 10) (100 200 20)) ; 100-109
                     '(99 15))                  ; 99-113
          '((0 10) (99 1) (110 4))))

  (5a:is (equal  ; seed-contains
          (map-seed '((0 60 5) (200 300 200))
                    '(50 20))
          '((0 5) (50 10) (65 5))))

  (5a:is (equal  ; no-overlap
          (map-seed '((200 100 200) (200 300 200))
                    '(0 100))
          '((0 100))))

  (5a:is (equal  ; no overlap
          (map-seed  '((0 100 10) (100 200 20))
                     '(300 100))
          '((300 100)))))
