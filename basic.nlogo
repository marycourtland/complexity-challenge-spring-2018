globals [
  ;; Each pool is represented by a row
  pool-height
  pool-length ;; should be divisible by N
  left-margin
  x-step-size

  ;; History lists of what happened each turn
  ;; shaped like [[x x x] [x x x] [x x x]]
  agent-history
  payoff-history ;; PER AGENT

  ;; misc
]

turtles-own [
  pool
  my-choice-history
  my-payoff-history
  my-cost-history
  my-data
  my-data-names

  strategy ;; nyi
]


to setup
  clear-all

  set agent-history []
  set payoff-history []

  display-world

  set-default-shape turtles "bug"

  create-turtles N [
    set color white
    set my-choice-history []
    set my-payoff-history []
    set my-cost-history []

    set my-data []
    set my-data-names []

    ;; Set the agent's strategy!

    ;;ifelse (random-float 1) < p [ set strategy 0 ] [ set strategy 1 ]

    set strategy one-of range 10

    ;;set strategy who mod 2
    ;;set strategy 1
  ]

  reset-ticks
end

to go
  ;; Agents decide which pools to go to next
  ask turtles [
    pick-next-pool
    move-to-pool
    deduct-move-cost
  ]

  ;; Count how many agents choose each pool
  let current-counts map [ pool-num -> count turtles with [pool = pool-num] ] [0 1 2]
  set agent-history (record agent-history current-counts)

  ;; Pools decide how much they'll pay out this round
  let current-payoffs map determine-current-payoff [0 1 2]
  set payoff-history (record payoff-history current-payoffs)

  ;; Agents get their reward
  ask turtles [
    collect-payoff
  ]

  tick
end

to display-world
  set-patch-size 15
  set pool-height 5
  ;;set pool-length 20
  set pool-length x-step-size * 100
  resize-world 0 (pool-length + left-margin) 0 (pool-height * 3 )

  let pool-colors [green blue red]
  let pool-labels ["Pool 0: STABLE" "Pool 1: LOW" "Pool 2: HIGH"]

  ;; color the rows representing pools
  foreach [0 1 2] [ pool-num ->
    let pool-patches patches with [pxcor > left-margin and (pycor >= (pool-num * pool-height) and pycor <= ((pool-num + 1) * pool-height))]
    ask pool-patches [ set pcolor (item pool-num pool-colors) ]
    ask patch (left-margin - 1) (pool-num * pool-height) [ set plabel (item pool-num pool-labels) ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HELPER METHODS
;;

to-report random-pool
  report one-of [0 1 2]
end

to-report record [history-list value]
  report insert-item 0 history-list value
end

;; (get-for-pool current-payoffs 0) -> 1
;; (get-for-pool first payoff-history 0) -> 1
to-report get-for-pool [pool-values-list pool-num]
  let pool-count 0
  carefully [
    set pool-count item pool-num pool-values-list
  ][]
  report pool-count
end

to-report probability-if [prob value1 value2]
  let dice-roll random-float 1
  ifelse dice-roll < prob [ report value1 ] [ report value2 ]
end

;; similar to (sublist 0 n), but returns the whole list if the position > list length
;; (instead of throwing error)
to-report first-x-of [the-list x]
  report sublist the-list 0 (min list x length the-list)
end

to-report payoff-history-for-pool [the-pool]
  report map [ values -> item the-pool values] payoff-history
end

to-report agent-history-for-pool [the-pool]
  report map [ values -> item the-pool values] agent-history
end

to-report recent-agent-history
  ifelse length agent-history > 0
  [ report first agent-history ]
  [ report [0 0 0] ]
end


to plot-agents-wealth
  ask turtles [
    set-current-plot-pen (word who)
    plot wealth
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SYSTEM MEASURES
;;

to-report total-wealth
  report sum [wealth] of turtles
end

to-report agent-variance
  let min-rounds 10
  if ticks < min-rounds [ report 0 ]
  let recent-counts sublist agent-history 0 min-rounds
  let recent-pool-0 map [turn-counts -> item 0 turn-counts] recent-counts
  let recent-pool-1 map [turn-counts -> item 1 turn-counts] recent-counts
  let recent-pool-2 map [turn-counts -> item 2 turn-counts] recent-counts
  report mean (list (variance recent-pool-0) (variance recent-pool-1) (variance recent-pool-2))
end

to-report information-entropy [ values possibilities ]
  ;; estimate probabilities from the frequencies?
  let counts n-values (length possibilities) [0]
  let total length values

  foreach values [v ->
    let index position v possibilities
    if (index = false) [ error "Value not in possibilities" ]
    let current-count item index counts
    let next-count (current-count + 1)
    set counts replace-item index counts next-count
  ]

  let information -1 * sum (map [ current-count ->
    ifelse-value (current-count = 0) [ 0 ] [
      (current-count / total) * ln (current-count / total)
    ]
  ] counts)

  report information
end

to-report mutual-information [values1 values2 possibilities]
  let info1 information-entropy values1 possibilities
  let info2 information-entropy values2 possibilities

  let combo sentence values1 values2
  let infoCombo information-entropy combo possibilities

  report info1 + info2 - infoCombo
end

to-report agent-choices-mutual-information [agent1 agent2]
  let choices1 [my-choice-history] of agent1
  let choices2 [my-choice-history] of agent2
  report mutual-information choices1 choices2 [0 1 2]
end

;; Individual turtle measures

to-report stable-choice-percent
  report (length filter [i -> i = 0] my-choice-history) / (length my-choice-history)
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; POOLS
;;

to-report determine-current-payoff [pool-num]
  let agent-count item pool-num (first agent-history) ;; it should've already been recorded now

  if pool-num = 0 [
    report 1
  ]

  if pool-num = 1 [
    let total-winnings (probability-if 0.5 40 0)
    if agent-count = 0 [ report total-winnings ]
    report total-winnings / agent-count
  ]

  if pool-num = 2 [
    let total-winnings (probability-if 0.25 80 0)
    if agent-count = 0 [ report total-winnings ]
    report total-winnings / agent-count
  ]

  error "bad pool number?"
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; TURTLES
;;

;; helpers to get named variables from 'my-data'... so that each strategy doesn't
;; have to add its variables to the turtle local vars
to-report get-data [name]
  if not member? name my-data-names [ report 0 ]
  let var-position position name my-data-names
  report item var-position my-data
end

to set-data [name value]
  if not member? name my-data-names [
    set my-data-names lput name my-data-names
    set my-data lput 0 my-data
  ]
  let var-position position name my-data-names
  set my-data replace-item var-position my-data value
end

to pick-next-pool
  ;;set pool one-of [0 1 2]

  if strategy = 0 [ set pool strategy-random ]
  if strategy = 1 [ set pool strategy-sitting-duck 0 ]
  if strategy = 2 [ set pool strategy-sitting-duck 1 ]
  if strategy = 3 [ set pool strategy-sitting-duck 2 ]
  if strategy = 4 [ set pool strategy-slow-random 5 ]
  if strategy = 5 [ set pool strategy-check-last-round ]
  if strategy = 6 [ set pool strategy-favor-stable 5 10]
  if strategy = 7 [ set pool adaptive-strategy-1 ]
  if strategy = 8 [ set pool adaptive-strategy-2 ]
  if strategy = 9 [ set pool strategy-turn-taker ]

  ;;if strategy = 0 [ set pool strategy-sitting-duck 1 ]
  ;;if strategy = 1 [ set pool strategy-sitting-duck 2 ]

  ;;if strategy = 0 [ set pool strategy-sitting-duck 1 ]
  ;;if strategy = 1 [ set pool strategy-random ]
  ;;if strategy = 2 [ set pool strategy-random ]
  ;;if strategy = 3 [ set pool strategy-check-last-round ]

  ;;if strategy = 0 [ set pool strategy-favor-stable 5 10]
  ;;if strategy = 1 [ set pool strategy-random ]

  ;;if strategy = 0 [ set pool strategy-favor-stable 2 7 ]
  ;;if strategy = 1 [ set pool strategy-random ]


  ;;if strategy = 0 [ set pool adaptive-strategy-1 ]
  ;;if strategy = 0 [ set pool adaptive-strategy-1-stochastic 0.2 ]
  ;;if strategy = 1 [ set pool strategy-favor-stable 5 10 ]

  ;;if strategy = 0 [ set pool adaptive-strategy-1 ]
  ;;if strategy = 1 [ set pool adaptive-strategy-2 ]

  ;;if strategy = 0 [ set pool strategy-turn-taker ]
  ;;if strategy = 0 [ set pool strategy-favor-stable 5 10 ]
  ;;if strategy = 1 [ set pool adaptive-strategy-2 ]

  set my-choice-history (record my-choice-history pool)
end

to move-to-pool
  pen-down
  let new-x (xcor + x-step-size)
  let new-y (pool * pool-height) + (random pool-height)
  if ((ticks > 1) and (first my-choice-history) = (first (but-first my-choice-history))) [
    set new-y (pool * pool-height)
  ]
  let dist distancexy new-x new-y
  facexy new-x new-y
  fd dist
end

to move-to-pool-1
  let my-sub-row floor (who / pool-length)
  let x 7 + (who mod pool-length)
  let y (pool * pool-height) + my-sub-row
  move-to patch x y
end

to deduct-move-cost
  let cost 0
  if ticks != 0 [
    let current-pool (first my-choice-history)
    let previous-pool (first but-first my-choice-history)
    if current-pool != previous-pool [ set cost tau ]
  ]
  set my-cost-history (record my-cost-history cost)
end

to collect-payoff
  let current-payoffs (first payoff-history)
  let my-payoff (item pool current-payoffs)
  set my-payoff-history (record my-payoff-history my-payoff)
end

to-report wealth
  let total-payoff sum my-payoff-history
  let total-cost sum my-cost-history
  report total-payoff - total-cost
end

;; Strategy template for tournament
;; also can access my-data
to-report choose-strategy-ID [ low-payoff high-payoff low-number high-number my-payoffs my-choices ]
  ;;CODE CODE CODE CODE
  let pool_ 1
  report pool_
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Vector / matrix helper functions
;;

to-report matrix-dot-vector [AA vv]
  ;; matrix should be:  [ [column] [column] [column] ]
  let matrix-cols range (length AA)
  let matrix-rows range (length (item 0 AA))
  let xx n-values (length AA) [ 0 ]

  foreach matrix-rows [ row-num ->
    foreach matrix-cols [ col-num ->
      let matrix-col item col-num AA
      let matrix-entry item row-num matrix-col
      let vector-entry item col-num vv
      let xx-entry item row-num xx
      set xx (replace-item row-num xx (xx-entry + (matrix-entry * vector-entry)))
    ]
  ]

  report xx
end

to-report vector-add [uu vv]
  ;; assume they are the same dimension
  let xx []
  foreach range length uu [ i ->
    let uui item i uu
    let vvi item i vv
    set xx lput (uui + vvi) xx
  ]
  report xx
end


to-report vector-diff [uu vv]
  ;; assume they are the same dimension
  let xx []
  foreach range length uu [ i ->
    let uui item i uu
    let vvi item i vv
    set xx lput (uui - vvi) xx
  ]
  report xx
end

to-report vector-sumsquares [vv]
  let sumsquares 0
  foreach range length vv [ i ->
    let vvi item i vv
    set sumsquares sumsquares + (vvi * vvi)
  ]
  report sumsquares
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Naive agents
;;

to-report strategy-sitting-duck [ the-pool ]
  report the-pool
end

to-report strategy-random
  report random-pool
end

to-report strategy-slow-random [ period ]
  ifelse ticks mod period = 0
  [ report random-pool ]
  [ report pool ]
end

to-report strategy-check-last-round
  ;; pick the pool with the lowest population last time
  ;; note: if there's a tie, it will choose the lower risk round. (What if that was random?)
  if ticks = 0 [ report random-pool ]
  let previous-counts first agent-history
  let min-pool position (min previous-counts) previous-counts
  report min-pool
end

to-report strategy-favor-stable [rest-min rest-max]
  ;; my-data = X, where X is the number of rounds I'll stay in the stable pool. Should be > 2 to justify move costs

  ;; define these here.. they might or might not get used
  let new-X one-of (range rest-min rest-max)
  let new-risk-pool one-of [1 2]

  if ticks = 0 [
    ;; initialize
    set my-data new-X
    report 0
  ]
  if pool = 0 [
    let X my-data - 1 ;; decrement X
    set my-data X
    ifelse X != 0
    [ report 0 ] ;; stay in stable until the X countdown has finished
    [ report new-risk-pool ] ;; switch to one of the risk pools once X finishes
  ]
  if pool != 0 [
    ;; If I'm currently in one of the risk pools, stay there until a payoff occurs.
    ifelse first my-payoff-history > 0 [
      set my-data new-X ;; restart countdown
      report 0
    ][
      report pool ;; stay there
    ]
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Predictive and adaptive agents
;;

;; HELPER FUNCTIONS

;; get S, matrix of probabilities for an agent to switch from pool i to pool j
to-report get-switching-probabilities [var-name]
  let S []

  ifelse ticks = 0 [
    ;; initialization: set up switching probabilities (3x3 matrix)
    ;; set it up as, [ [column] [column] [column] ]
    ;; each column i is:
    ;; [
    ;;   probability for an agent to switch from pool i to pool 0
    ;;   probability for an agent to switch from pool i to pool 1
    ;;   probability for an agent to switch from pool i to pool 2
    ;; ]

    set S n-values 3 [ n-values 3 [ random-float 1 ] ]

    ;; normalize the columns (so that probabilities to 1)
    foreach [0 1 2] [ col-num ->
      let column item col-num S
      let total sum column
      let normalized-column map [ x -> x / total ] column
      set S replace-item col-num S normalized-column
    ]

    ;; store the matrix for future ticks
    if var-name != "" [ set-data var-name S ]
  ][
    set S get-data "S"
  ]

  report S
end

to-report mutate-switching-probabilities [ S magnitude ]
  let S-mutated []

  foreach [0 1 2] [col-num ->
    let column item col-num S

    let m0 ((random-float 1) * 2 - 1) * magnitude
    let m1 ((random-float 1) * 2 - 1) * magnitude
    let m2 0 - m0 - m1

    let mutated-column vector-add column (list m0 m1 m2)
    set S-mutated lput mutated-column S-mutated
  ]

  report S-mutated
end

to-report calculate-my-possible-payoffs [agent-pool-counts]
  let payoffs n-values 3 [ 0 ]
  foreach range 3 [ pool-num ->
    let agent-pool-count item pool-num agent-pool-counts
    if agent-pool-count = 0 [ set agent-pool-count 1 ] ;; avoid div by zero
    let payoff 0
    if pool-num = 0 [ set payoff 1 ]
    if pool-num = 1 [ set payoff 20 / agent-pool-count ] ;; expectation value
    if pool-num = 2 [ set payoff 20 / agent-pool-count ] ;; expectation value
    set payoffs replace-item pool-num payoffs payoff
  ]

  ;; discount tau from the payoffs
  if pool != 0 [ set payoffs replace-item 0 payoffs (item 0 payoffs - tau) ]
  if pool != 1 [ set payoffs replace-item 1 payoffs (item 1 payoffs - tau) ]
  if pool != 2 [ set payoffs replace-item 2 payoffs (item 2 payoffs - tau) ]

  report payoffs
end

;; STRATEGIES

to-report predictive-strategy-1
  let S get-switching-probabilities ["S"]

  if ticks = 0 [ report random-pool ]

  let agents-previous-turn first agent-history
  let agent-prediction matrix-dot-vector S agents-previous-turn
  let payoff-prediction calculate-my-possible-payoffs agent-prediction

  ;; let's go to the pool with highest predicted payoff
  let max-payoff max payoff-prediction
  report position max-payoff payoff-prediction
end

;; Same as predictive-strategy-1 but keeps a bunch of options in its back pocket.
to-report adaptive-strategy-1
  ;;let num-variants 10
  let S-variants []

  ifelse ticks = 0 [
    foreach range num-variants [
      set S-variants lput (get-switching-probabilities [""]) S-variants
    ]
    set-data "S-variants" S-variants
    set-data "S" first S-variants
  ][
    set S-variants get-data "S-variants"
  ]

  if ticks = 0 [ report random-pool ]
  if ticks = 1 [ report predictive-strategy-1 ] ;; this will use the first S matrix and also set it

  let S-min-error get-data "S"
  let min-error 9999

  ;; Check previous two rounds, and retroactively see which S matrix would have been best
  let agents-turn-1 first but-first agent-history
  let agents-turn-2 first agent-history

  ;; see what the max payoff would have been
  let actual-payoffs calculate-my-possible-payoffs agents-turn-2
  let actual-max-payoff max actual-payoffs

  foreach S-variants [ S ->

    ;; simulate prediction with this S
    let agents-predicted-turn-2 matrix-dot-vector S agents-turn-1
    let payoff-prediction calculate-my-possible-payoffs agents-predicted-turn-2

    let prediction-error vector-sumsquares (vector-diff agents-predicted-turn-2 agents-turn-2)

    if prediction-error < min-error [
      set min-error prediction-error
      set S-min-error S
    ]
    ;; ...

    ;;let predicted-max-payoff max payoff-prediction
    ;;let chosen-position position predicted-max-payoff payoff-prediction

    ;; if this S gave us the max payoff, then mark it as a good one to be used
    ;;if predicted-max-payoff = actual-max-payoff [
    ;;  set S-best-choices lput S S-best-choices
    ;;]
  ]

  set-data "error" min-error
  set-data "S" S-min-error
  let S S-min-error

  ;;if length S-best-choices > 0 [
  ;;  set S one-of S-best-choices
  ;;  set-data "S" S
  ;;]

  ;; now use this S matrix for our next choice

  let agents-previous-turn first agent-history
  let agent-prediction matrix-dot-vector S agents-previous-turn
  let payoff-prediction calculate-my-possible-payoffs agent-prediction

  ;; let's go to the pool with highest predicted payoff
  let max-payoff max payoff-prediction
  report position max-payoff payoff-prediction
end

;; Same as predictive-strategy-1 but with some stochasticity.
to-report adaptive-strategy-1-stochastic [random-probability]
  if ticks <= 2 [ report adaptive-strategy-1 ]

  ifelse (random-float 1) < random-probability [
    report random-pool
  ] [
    report adaptive-strategy-1
  ]
end


;; Same as predictive-strategy-1 but does some genetic variance of the strategies in its list
to-report adaptive-strategy-2
  if ticks < 2 [ report adaptive-strategy-1 ] ;; don't start varying S choices until tick 2

  let choice adaptive-strategy-1

  ;; replace some S choices with variants of my latest used S
  let num-replacements floor num-variants * percent-to-replace
  let indices-to-replace n-of num-replacements range num-variants
  let S-variants get-data "S-variants"
  let S get-data "S"

  foreach indices-to-replace [ i ->
    let mutated mutate-switching-probabilities S mutation-magnitude
    set S-variants replace-item i S-variants mutated
  ]
  set-data "S-variants" S-variants

  report choice
end


to-report strategy-turn-taker
  ;; The gist of this strategy is: each agent using this strategy will pick one turn
  ;; to go to the low pool, and one turn to go to the high pool. (Both with uniform
  ;; random probability.) This ensures that not-very-many agents per turn will go to
  ;; low or high.
  ;; Since the game will last for more than N turns, the agent will go to the pool if
  ;; it's their turn, modulo N.

  if ticks = 0 [
    set-data "turn-for-L" random N
    set-data "turn-for-H" random N
  ]
  let turn-for-L get-data "turn-for-L"
  let turn-for-H get-data "turn-for-H"

  ;; make sure one pool isn't favored if the turns are the same
  if (turn-for-L = turn-for-H) and (ticks mod N = turn-for-L) [ report one-of [1 2] ]

  if ticks mod N = turn-for-L [ report 1 ]
  if ticks mod N = turn-for-H [ report 2 ]
  report 0
end



to-report strategy-weighted-history [memory-size]
  report 0
end
@#$#@#$#@
GRAPHICS-WINDOW
327
10
350
259
-1
-1
15.0
1
10
1
1
1
0
0
0
1
0
0
0
15
0
0
1
ticks
30.0

SLIDER
12
17
184
50
tau
tau
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
14
74
186
107
N
N
0
100
50.0
5
1
NIL
HORIZONTAL

BUTTON
16
350
90
383
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
16
397
89
430
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
106
396
203
429
Go 1 Tick
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
530
302
767
486
Total Wealth
Rounds
Wealth
0.0
100.0
0.0
10000.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot total-wealth"

PLOT
794
30
1349
199
Agents choosing each pool
Round
Number of agents
0.0
100.0
0.0
50.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -10899396 true "" "plot get-for-pool recent-agent-history 0"
"pen-1" 1.0 0 -13345367 true "" "plot get-for-pool recent-agent-history 1"
"pen-2" 1.0 0 -2674135 true "" "plot get-for-pool recent-agent-history 2"

MONITOR
622
520
766
593
Total Wealth
total-wealth
1
1
18

PLOT
1149
377
1349
527
Wealth distribution
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" "set-histogram-num-bars 50\nset-plot-y-range 0 1\n  set-plot-x-range 0 (max [ wealth ] of turtles + 1)"
PENS
"default" 1.0 1 -16777216 true "" "histogram [wealth] of turtles"

PLOT
225
302
522
487
Wealth by group (per turtle)
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"group 0" 1.0 0 -8431303 true "" "plot (sum [wealth] of turtles with [ strategy = 0 ]) / (count turtles with [strategy = 0])"
"group 1" 1.0 0 -13840069 true "" "plot (sum [wealth] of turtles with [ strategy = 1 ]) / (count turtles with [strategy = 1])"
"group 2" 1.0 0 -13791810 true "" "plot (sum [wealth] of turtles with [ strategy = 2 ]) / (count turtles with [strategy = 2])"
"group 3" 1.0 0 -5825686 true "" "plot (sum [wealth] of turtles with [ strategy = 3 ]) / (count turtles with [strategy = 3])"

BUTTON
18
445
203
478
Setup & Go 100 ticks
setup\nrepeat 100 [ go ]\nshow map [i -> precision ((mean [wealth] of turtles with [strategy = i]) / ticks) 3 ] range 10
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
131
350
201
383
reset
set tau 0.5\nset N 50
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
459
521
603
594
Mean wealth
mean [wealth] of turtles
2
1
18

SLIDER
15
121
187
154
p
p
0
1
0.0
0.05
1
NIL
HORIZONTAL

PLOT
809
562
1318
817
Individual agents wealth
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"ask turtles [\ncreate-temporary-plot-pen (word who)\n]" "ask turtles [\n    set-current-plot-pen (word who)\n    set-plot-pen-color ((strategy + 1) * 30 + 4)\n    plot wealth\n  ]"
PENS
"default" 1.0 0 -16777216 true "" ""

SLIDER
16
207
188
240
num-variants
num-variants
0
20
1.0
1
1
NIL
HORIZONTAL

PLOT
794
209
1352
329
Payoff per pool
NIL
NIL
0.0
100.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -13840069 true "" "plot item 0 (first payoff-history)"
"pen-1" 1.0 0 -13345367 true "" "plot item 1 (first payoff-history)"
"pen-2" 1.0 0 -2674135 true "" "plot item 2 (first payoff-history)"

SLIDER
15
252
190
285
percent-to-replace
percent-to-replace
0
1
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
15
298
192
331
mutation-magnitude
mutation-magnitude
0
0.5
1.0
0.05
1
NIL
HORIZONTAL

PLOT
801
376
1121
526
Agent variance, last 10 turns
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot agent-variance"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="vary tau" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>total-wealth</metric>
    <metric>sum [wealth] of turtles with [ strategy = 0 ]</metric>
    <metric>sum [wealth] of turtles with [ strategy = 1 ]</metric>
    <metric>sum [wealth] of turtles with [ strategy = 2 ]</metric>
    <metric>sum [wealth] of turtles with [ strategy = 3 ]</metric>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tau" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="p">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vary p, tau=0" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>total-wealth</metric>
    <metric>sum [wealth] of turtles with [ strategy = 0 ]</metric>
    <metric>sum [wealth] of turtles with [ strategy = 1 ]</metric>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="p" first="0" step="0.05" last="1"/>
  </experiment>
  <experiment name="vary tau, measure pool choices" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>total-wealth</metric>
    <metric>get-for-pool recent-agent-history 0</metric>
    <metric>get-for-pool recent-agent-history 1</metric>
    <metric>get-for-pool recent-agent-history 2</metric>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tau" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="p">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vary tau and p" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>total-wealth</metric>
    <metric>sum [wealth] of turtles with [ strategy = 0 ]</metric>
    <metric>sum [wealth] of turtles with [ strategy = 1 ]</metric>
    <metric>sum [wealth] of turtles with [ strategy = 2 ]</metric>
    <metric>sum [wealth] of turtles with [ strategy = 3 ]</metric>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tau" first="0" step="0.05" last="1"/>
    <steppedValueSet variable="p" first="0" step="0.05" last="1"/>
  </experiment>
  <experiment name="vary tau and num-variants" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>total-wealth</metric>
    <metric>sum [wealth] of turtles with [ strategy = 0 ]</metric>
    <metric>agent-variance</metric>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tau" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="p">
      <value value="0"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num-variants" first="0" step="1" last="20"/>
    <enumeratedValueSet variable="percent-to-replace">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-magnitude">
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vary tau and percent-to-replace" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>total-wealth</metric>
    <metric>sum [wealth] of turtles with [ strategy = 0 ]</metric>
    <metric>agent-variance</metric>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tau" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="p">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-variants">
      <value value="5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="percent-to-replace" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="mutation-magnitude">
      <value value="0.2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vary tau, N" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>total-wealth</metric>
    <metric>sum [wealth] of turtles with [ strategy = 0 ]</metric>
    <metric>sum [wealth] of turtles with [ strategy = 1 ]</metric>
    <metric>sum [wealth] of turtles with [ strategy = 2 ]</metric>
    <metric>sum [wealth] of turtles with [ strategy = 3 ]</metric>
    <steppedValueSet variable="N" first="20" step="10" last="100"/>
    <steppedValueSet variable="tau" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="p">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="vary mutation-magnitude" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>total-wealth</metric>
    <metric>get-for-pool recent-agent-history 0</metric>
    <metric>get-for-pool recent-agent-history 1</metric>
    <metric>get-for-pool recent-agent-history 2</metric>
    <enumeratedValueSet variable="percent-to-replace">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="mutation-magnitude" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="tau">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-variants">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
