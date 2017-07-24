
(define NEWPLAYER (list))

(define (_addNewPlayer id)
    (set! NEWPLAYER (append NEWPLAYER (list id)))
)

(define (getNewestPlayer)
    (if (> (length NEWPLAYER) 0)
        (lambda ()
            ; New Player
        )
    )
)