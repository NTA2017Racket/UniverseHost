#lang racket

(require "Struct.rkt")
(require "TCPEvents.rkt")

(define _EVENTS (list))

(define (_addNewPlayer id)
    (set! _EVENTS (append _EVENTS (TcpEvent PLAYERJOINED id "")))
)

(define (_removePlayer id)
    (set! _EVENTS (append _EVENTS (TcpEvent PLAYERLEFT id "")))
)

(define (_PlayerHasChangedName id newName)
    (set! _EVENTS (append _EVENTS (TcpEvent PLAYERHASCHANGEDNAME id newName)))
)

(define (getLatestEvent)
    _EVENTS
    (set! _EVENTS (list))
)

(provide getLatestEvent _addNewPlayer _removePlayer _PlayerHasChangedName)