#lang racket

(require racket/udp)
(require racket/match)
(require racket/cmdline)

(require "json-util.rkt")

(define game-name (make-parameter "Raketti Botti"))
(define game-id (make-parameter "rakettigame"))
(define game-color (make-parameter "red"))
(define remote-addr (make-parameter "127.0.0.1"))
(define remote-port (make-parameter 4567))

(struct game-data (name
                   id
                   color
                   connection-id
                   lobby-addr
                   lobby-port
                   game-addr
                   game-port
                   socket))

;; mm.
(define (init-game-data)
  (game-data (game-name)
             (game-id)
             (game-color)
             -1
             (remote-addr)
             (remote-port)
             ""
             -1
             (udp-open-socket)))

(define (game-server gd)
  (list (game-data-game-addr gd)
        (game-data-game-port gd)))

(define (lobby-server gd)
  (list (game-data-lobby-addr gd)
        (game-data-lobby-port gd)))

(define (hellomsg gd)
  (hasheq 'type "connect"
          'name (game-data-name gd)
          'game-id (game-data-id gd)))

(define (pongmsg cid) (hasheq 'type "pong"
                              'connection-id cid))

(define (game-join-msg gd)
  (hasheq 'type "join"
          'connection-id (game-data-connection-id gd)
          'name (game-data-name gd)
          'color (game-data-color gd)
          'game-id (game-data-id gd)
          'players (list (hasheq 'number 1 'name "John")
                         (hasheq 'number 2 'name "Paul")
                         (hasheq 'number 3 'name "George")
                         (hasheq 'number 3 'name "Richard"))))

(define (run-game)
  (let ([gd (init-game-data)])
    (udp-bind! (game-data-socket gd) #f 0 #f)
    (send-hello gd)
    (game-loop gd)
    (udp-close (game-data-socket gd))))

(define (game-loop gd)
  (let*-values ([(bytes) (make-bytes 10000)]
                [(length ip port) (udp-receive! (game-data-socket gd)
                                                bytes)])
    (let ([new-gd (handle-msg gd bytes length ip port)])
      (when (game-data? new-gd)
        (game-loop new-gd)))))

(define (handle-msg gd bytes length ip port)
  (printf "Received ~a bytes from ~a:~a: [~a]\n" length ip port bytes)
  (let ([source (list ip port)])
    (cond
     [(equal? source (lobby-server gd)) (handle-lobby-msg gd bytes)]
     [(equal? source (game-server gd)) (handle-game-msg gd bytes)]
     [else gd])))

(define (handle-lobby-msg gd bytes)
  (let ([jsonm (bytes->json bytes)])
    (match (hash.refs jsonm.type)
      ["connect-ok" (set-connection-id gd jsonm)]
      ["ping" (send-pong gd)]
      ["game-at" (start-game gd jsonm)]
      ["goodbye" #f]
      [_ gd])))

(define (set-connection-id gd jsonm)
  (let ([cid (hash.refs jsonm.connection-id)])
    (struct-copy game-data gd [connection-id cid])))

(define (send-hello gd)
  (send-to gd (lobby-server gd) (hellomsg gd)))

(define (send-pong gd)
  (send-to gd (lobby-server gd) (pongmsg (game-data-connection-id gd)))
  gd)

(define (start-game gd jsonm)
  (let ([gd2 (struct-copy game-data gd
                          [game-addr (hash.refs jsonm.address)]
                          [game-port (hash.refs jsonm.port)]
                          [id (hash.refs jsonm.game-id)])])
    (send-game-join gd2)))

(define (handle-game-msg gd bytes)
  (printf "Handle game msg from ~a:~a : [~a]\n"
          (game-data-game-addr gd)
          (game-data-game-port gd)
          bytes)
  gd)

(define (send-game-join gd)
  (send-to gd (game-server gd) (game-join-msg gd))
  gd)

(define (send-to gd destination jsonmsg)
  (let ([bytes (json->bytes jsonmsg)]
        [addr (car destination)]
        [port (cadr destination)])
    (printf "Sending to ~a:~a [~a]\n" addr port bytes)
    (udp-send-to (game-data-socket gd) addr port bytes)))

(module+ main
  (command-line
   #:once-each
   [("-n" "--name") n "Game name" (game-name n)]
   [("-i" "--id") i "Game id" (game-id i)]
   [("-c" "--color") c "Color" (game-color c)]
   [("-a" "--addr") a "Remote address" (remote-addr a)]
   [("-p" "--port") p "Remote port" (remote-port (string->number p))])
  (run-game))
