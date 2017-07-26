#lang racket

(require "Struct.rkt")
(require "TCPEvents.rkt")
(require "Constants.rkt")

(define _EVENTS (list))
(define CLIENTLIST (make-hash))
(define CLIENTNAMELIST (make-hash))

(define (_addNewPlayer id)
    (dict-set! CLIENTNAMELIST id DEFAULTNAME)
    (set! _EVENTS (append _EVENTS (list (TcpEvent PLAYERJOINED id ""))))
)

(define (_removePlayer id)
    (dict-remove! CLIENTNAMELIST id)
    (set! _EVENTS (append _EVENTS (list (TcpEvent PLAYERLEFT id ""))))
)

(define (_PlayerHasChangedName id newName)
    (dict-set! CLIENTNAMELIST id newName)
    (set! _EVENTS (append _EVENTS (list (TcpEvent PLAYERHASCHANGEDNAME id newName))))
)

(define (_resolvePlayername id)
    (dict-ref CLIENTNAMELIST id)
)

(define (nameAndID id)
    (string-append "[" (_resolvePlayername id) "] ")
)

(define (_PlayerShoot id degree)
    (set! _EVENTS (append _EVENTS (list (TcpEvent PLAYERSHOOT id degree))))
)

(define (getLatestEvent)
    _EVENTS
    (set! _EVENTS (list))
)

(provide getLatestEvent nameAndID _PlayerShoot _resolvePlayername CLIENTLIST _addNewPlayer _removePlayer _PlayerHasChangedName)