#lang racket
(require "structs.rkt")
(provide jsexpr/player->race-result
         jsexpr/results->stage)

(define (jsexpr/player->race-result j p)
  (match j
    [#f (race-result p
                     #f
                     #f)]
    [_ (race-result p
                    (hash-ref j 'VehicleName)
                    (string->time (hash-ref j 'Time)))]))

(define (jsexpr/results->stage j results)
  (define name (hash-ref j 'StageName))
  (define h `#hasheq((retry . ,(hash-ref j 'StageRetry))
                     (service . ,(hash-ref j 'HasServiceArea))
                     (engineers . ,(hash-ref j 'AllowCareerEngineers))
                     (only-owned-vehicles . ,(hash-ref j 'OnlyOwnedVehicles))
                     (tuning . ,(hash-ref j 'AllowVehicleTuning))
                     (checkpoint . ,(hash-ref j 'IsCheckpoint))
                     (wager . ,(hash-ref j 'IsWagerEvent))
                     (location . ,(hash-ref j 'LocationName))
                     (name . ,name)
                     (stage . ,(hash-ref j 'StageName))
                     (time-of-day . ,(hash-ref j 'TimeOfDay))
                     (weather . ,(hash-ref j 'WeatherText))
                     (restriction . ,(hash-ref j 'Restriction))
                     (restart . ,(hash-ref j 'EventRestart))))
  (stage name h results))