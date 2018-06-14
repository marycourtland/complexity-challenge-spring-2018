extensions [ nw ]

globals [
  ;; Each pool is represented by a row
  pool-height
  pool-length ;; should be divisible by N
  left-margin
  x-step-size
  strategy-colors

  num-strategies

  ;; History lists of what happened each turn
  ;; shaped like [[x x x] [x x x] [x x x]]
  agent-history
  payoff-history ;; PER AGENT


  ;; GA variables
  param-population-mutations
  current-mutation
  mutation-history ;; list of [fitness, generation, mutation]. MOST RECENT AT END
  generation-count
  current-repeat

  pop
  temp-pop
  temp-s

]

turtles-own [
  pool
  my-choice-history
  my-payoff-history
  my-cost-history
  my-data
  my-data-names
  strategy
  group
]


to setup

  ;; persist these globals across multiple runs
  let saved-current-repeat current-repeat
  let saved-pop pop
  let saved-param-population-mutations param-population-mutations
  let saved-current-mutation current-mutation
  let saved-mutation-history mutation-history
  let saved-generation-count generation-count
  clear-all
  set param-population-mutations saved-param-population-mutations
  set current-mutation saved-current-mutation
  set mutation-history saved-mutation-history
  set generation-count saved-generation-count
  set pop saved-pop
  set current-repeat saved-current-repeat

  let current-param-population []
  if use-meta-ga? and not is-list? pop [
    iterate-parameter-population
    set current-param-population (item current-mutation param-population-mutations)


    if print-ga-to-console? [
      type "Running mutation "
      print current-mutation
      show nested-precision (current-param-population) 2
    ]
  ]

  set num-strategies 16

  set agent-history []
  set payoff-history []

  display-world

  set-default-shape turtles "person"


  ifelse (use-sharing? and not (use-groups? and num-groups > 0)) [
    nw:generate-watts-strogatz turtles links N 2 0.5
    ;;nw:generate-preferential-attachment turtles links N 3
  ] [
    create-turtles N
  ]

  ask turtles [
    set color white
    set my-choice-history []
    set my-payoff-history []
    set my-cost-history []

    set my-data []
    set my-data-names []

    ;; Set the agent's strategy!
    ifelse (random-float 1) > p [ set strategy strategy1 ] [ set strategy strategy2 ]
    ;;set strategy 0

    if use-tournament-turtle and who = 0 [
      set strategy 99
    ]

    ifelse (use-groups? and num-groups > 0) [
      set group one-of range num-groups
    ]
    [
      set group -1
    ]

    if use-meta-ga? [
      ;; assign one of the params from the population to this agent
      ifelse not is-list? pop [
        set-data "params" item who current-param-population
        set-data "biases" random-normalized-vector 3
      ] [
        ;; Overrides whatever current-param-population is. (Which was the output of the ga mutation)
        set-data "params" item who pop
        set-data "biases" random-normalized-vector 3
      ]
    ]
  ]


  if (use-groups? and num-groups > 0) [
    ask turtles [
      let my-group group
      let me-who who
      create-links-with (turtles with [ group = my-group and who != me-who ])
    ]
  ]

  reset-ticks
end

to assign-params-to-turtles [params]
  ask turtles [
    ;; assign one of the params from the population to this agent
    set-data "params" item who params
    set-data "biases" random-normalized-vector 3
  ]
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

  display-payoff-on-patches

  ask links [
    share-models
  ]

  ;; Agents get their reward
  ask turtles [
    collect-payoff
  ]

  tick
end

to finish-game
  if use-meta-ga? [

    if print-ga-to-console? [
      type "Fitness: "
      print round(get-current-fitness)
    ]
    record-mutation-fitness
  ]
end

to display-world
  set left-margin 6
  set x-step-size 3
  set-patch-size 15
  set pool-height 5
  ;;set pool-length 20
  set pool-length x-step-size * 100
  resize-world 0 (pool-length + left-margin) 0 (pool-height * 3 )

  ;;let pool-colors [green blue red]
  let pool-colors [68 108 18]
  let pool-labels ["Pool 0: STABLE" "Pool 1: LOW" "Pool 2: HIGH"]

  ;; for plotting strategies etc
  set strategy-colors [0 25 22 32 42 72 82 112 122 132  35 45 75 85 115 125 135]

  ;; color the rows representing pools
  foreach [0 1 2] [ pool-num ->
    let pool-patches patches with [pxcor > left-margin and (pycor >= (pool-num * pool-height) and pycor < ((pool-num + 1) * pool-height))]
    ask pool-patches [ set pcolor (item pool-num pool-colors) ]
    ask patch (left-margin - 1) (pool-num * pool-height + 2) [ set plabel (item pool-num pool-labels) ]
  ]
end

to display-payoff-on-patches
  ;; show if low and high pools got lucky this turn.
  let pool-colors [67 107 17]
  foreach [1 2] [ pool-num ->
    if item pool-num first payoff-history > 0 [
      let pool-patches patches with [pxcor >= (left-margin + 1 + ticks * x-step-size) and pxcor < (left-margin + 1 + (ticks + 1) * x-step-size) and (pycor >= (pool-num * pool-height) and pycor < ((pool-num + 1) * pool-height))]
      ask pool-patches [ set pcolor (item pool-num pool-colors) ]
    ]
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PARAMETER POPULATIONS (genetic algorithm)
;;

to iterate-parameter-population
  ifelse not is-list? param-population-mutations or length param-population-mutations != num-param-pop-mutations [
    ;; At the start: the populations have not been initialized yet
    ;; 1. Spawn a random set of populations
      if print-ga-to-console? [
        print "** Spawning Initial Populations **"
      ]
    set param-population-mutations n-values num-param-pop-mutations [ spawn-population ]
    set mutation-history []
    set generation-count 0

    ;; 2. We'll start by evaluating the first one
    set current-mutation 0
    set current-repeat 0
  ] [
    ;; Repeat 3 times
    set current-repeat current-repeat + 1

    ;; Between runs: we try out each mutation

    ;; 1. Try the next mutation
    set current-mutation current-mutation + 1

    ;; Check if it's time to mutate.
    if current-mutation = num-param-pop-mutations [
      ;; Apply a mutation!
      set generation-count generation-count + 1

      let hlength length mutation-history
      let last-mutations sublist mutation-history (hlength - num-param-pop-mutations) hlength

      ;; choose the best to mutate
      ;; NOTE: the result of the reduce will be in the form of [fitness, mutation]

      let best-mutation reduce [[best-so-far next] ->
        ifelse-value ((first best-so-far) > (first next)) [ best-so-far ] [ next ]
      ] last-mutations

      if print-ga-to-console? [
        print "**"
        type "The best mutation had fitness "
        print round first best-mutation
        print nested-precision (last best-mutation) 2
        print "Mutating it!"
        print "**"
        type "Generation "
        print generation-count
        print "**"
      ]

      let num-mutations-to-keep clamp keep-mutations 0 (num-param-pop-mutations - 1)
      let mutations-to-keep n-of (clamp keep-mutations 0 (num-param-pop-mutations - 1)) param-population-mutations ;; note: this list might include best-mutation

      ;; spawn copies of the best...
      set param-population-mutations n-values (num-param-pop-mutations - num-mutations-to-keep) [ last best-mutation ]

      ;; ...and then mutate all of them
      set param-population-mutations (map mutate-population param-population-mutations)

      ;; And keep the ones we want to stick around.
      set param-population-mutations sentence param-population-mutations mutations-to-keep

      ;; After mutating, we will start over by evaluating the first mutation
      set current-mutation 0
    ]
  ]
end

to-report spawn-population
  report n-values N [ choose-random-parameters ]
end

;; random parameters, for strategy-parameterized, for a single agent
to-report choose-random-parameters
  report random-normalized-vector 4
end

to-report mutate-population [ param-population ]
  let mutated-population []

  ;; We will apply the same adjustment to each member in the population.
  let index-to-adjust one-of range 4 ;; just one of the first four entries
  let adjustment 1 + random-normal 0 mutation-magnitude

  foreach param-population [ params ->
    ;; only some of the turtles will mutate
    ifelse ((random-float 1) < mutation-probability) [

      ;; We will pick the entry that is going to be adjusted,
      ;; then calculate the adjusted value
      let value-to-adjust item index-to-adjust params
      let adjusted-value (value-to-adjust * adjustment)
      set adjusted-value min list 1 adjusted-value
      set adjusted-value max list 0 adjusted-value

      ;; The rest of the entries are going to be 'squeezed' or 'expanded' to fit.
      let mutated []
      ifelse (value-to-adjust != 1) [
        let remainder-before 1 - value-to-adjust
        let remainder-after 1 - adjusted-value
        let scale remainder-after / remainder-before

        set mutated n-values length params [ i ->
          ifelse-value (i = index-to-adjust) [ adjusted-value ] [ (item i params) * scale ]
        ]
      ][
        ;; This block will handle the edge case where value-to-adjust was 100% of the choice.
        ;; (The other probabilities were all 0, so give them an equal share.)
        let remainder-after 1 - adjusted-value
        let remainder-apportioned (remainder-after / (4 - 1))

        set mutated n-values length params [ i ->
          ifelse-value (i = index-to-adjust) [ adjusted-value ] [ remainder-apportioned ]
        ]
      ]

      set mutated-population lput mutated mutated-population
    ] [
      ;; this params isn't going to mutate.
      set mutated-population lput params mutated-population
    ]
  ]

  report mutated-population
end

to record-mutation-fitness
  if is-list? param-population-mutations and not is-list? pop [
    let current-param-population (item current-mutation param-population-mutations)
    let turtle-wealths  map  [i -> [wealth] of turtle i ] range N ;; [wealth] of turtles is not sorted.
    set mutation-history lput (list get-current-fitness generation-count turtle-wealths current-param-population) mutation-history
  ]
end

to-report get-current-fitness
  report ifelse-value use-gini-in-fitness? [
    total-wealth * (1 - gini-index [wealth] of turtles)
  ] [
    total-wealth
  ]
end

to display-mutation-history
  foreach mutation-history [ mh ->
    type first mh
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

to-report nested-precision [ array2d decimal-places ]
  report map [i -> map [j -> precision j decimal-places ] i] array2d
end

to-report clamp [i clamp_min clamp_max]
  set i min list i clamp_max
  set i max list i clamp_min
  report i
end

to-report is-switching-matrix-equal? [ s1 s2 ]
  let equal true
  foreach range 3 [i ->
    let col1 item i s1
    let col2 item i s2
    foreach range 3 [j ->
      if (item j col1) != (item j col2) [ set equal false ]
    ]
  ]
  report equal
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; SYSTEM MEASURES
;;

to-report total-wealth
  report sum [wealth] of turtles
end

to-report wealth-per-turtle-with-strategy [the-strategy]
  report (sum [wealth] of turtles with [ strategy = the-strategy ]) / (count turtles with [strategy = the-strategy])
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

to-report gini-index [wealths]
  let sorted-wealths sort wealths
  let nn length wealths

  let numerator 0
  foreach range nn [ i ->
    set numerator numerator + (2 * (i + 1) - nn - 1) * (item i sorted-wealths)
  ]
  let denominator (mean wealths) * nn * nn
  report numerator / denominator
end

;; Individual turtle measures

to-report stable-choice-percent
  report (length filter [i -> i = 0] my-choice-history) / (length my-choice-history)
end

to-report total-pool-counts
  report n-values 3 [ pool-num ->
    length filter [ i -> i = pool-num ] my-choice-history
  ]
end

;; Reports the matrix where the entry in column i and row j
;; is the number of times the agent switched from pool i to pool j.
;; (Same format as the switching-probabilities matrix)
to-report switch-counts
  let switches n-values 3 [ n-values 3 [ 0 ]]

  let prev-pool last my-choice-history
  foreach reverse but-last my-choice-history [ next-pool ->
    let prev-pool-column (item prev-pool switches)
    let counts item next-pool prev-pool-column
    let col2 replace-item next-pool prev-pool-column (counts + 1)
    set switches replace-item prev-pool switches col2
    set prev-pool next-pool
  ]

  report switches
end


to-report flatten-2d-array [ array ]
  let output []
  foreach array [col ->
    foreach col [ entry ->
      set output lput entry output
    ]
  ]
  report output
end

;; python friendly array output!
to disp [array]
  type "["
  foreach array [ i ->
    type i
    type ", "
  ]
    print "]"
end

;;foreach [total-pool-counts] of turtles [ poolcounts -> foreach poolcounts [ i -> type i]  ]

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
  ifelse use-meta-ga? [
    set pool strategy-parameterized (get-data "params") (get-data "biases")
  ][
    ;;set pool one-of [0 1 2]

    if strategy = 0 [ set pool strategy-random ]
    if strategy = 1 [ set pool strategy-sitting-duck 0 ]
    if strategy = 2 [ set pool strategy-sitting-duck 1 ]
    if strategy = 3 [ set pool strategy-sitting-duck 2 ]
    if strategy = 4 [ set pool strategy-slow-random 5 ]
    if strategy = 5 [ set pool strategy-check-last-round ]
    if strategy = 6 [ set pool strategy-favor-stable 5 10]
    if strategy = 7 [ set pool strategy-switching-matrix-1 ]
    if strategy = 8 [ set pool strategy-switching-matrix-2 ]
    if strategy = 9 [ set pool strategy-turn-taker ]
    if strategy = 10 [ set pool strategy-weighted-memory mem-size ]

    if strategy = 99 [
      let low-payoff map [i -> first but-first i ] payoff-history
      let high-payoff map [i -> last i ] payoff-history
      let low-number map [i -> first but-first i ] agent-history
      let high-number map [i -> last i ] agent-history
      let my-payoffs my-payoff-history
      let my-choices my-choice-history
      set pool choose-strategy-9  low-payoff high-payoff low-number high-number my-payoffs my-choices
    ]

    ;;set pool strategy-parameterized 0 0.1 0.6 0.3 0 0
    ;;if ticks = 0 [
    ;;  set-data "my-bias-pool" one-of [0 1 2]
    ;;]
    ;;let biases replace-item (get-data "my-bias-pool") [0 0 0] 1
    ;;let the-x (1 - the-b - the-r - the-h)
    ;;set pool strategy-parameterized the-b the-x the-r the-h (item 0 biases) (item 1 biases) (item 2 biases)
  ]

  set my-choice-history (record my-choice-history pool)
end

to move-to-pool
  let new-x ((xcor) + x-step-size)
  let new-y (pool * pool-height) + (random-float (pool-height - 1))

  set color item strategy strategy-colors

  ifelse ticks = 0
  [
    pen-up
    set new-x left-margin + 2
    setxy new-x new-y
  ]
  [
    pen-down
    if (first my-choice-history) = (first (but-first my-choice-history)) [
      set new-y ycor
    ]
    carefully [ setxy (new-x) new-y ] []
  ]
end

to move-to-pool-1
  let my-sub-row floor (who / pool-length)
  let x 7 + (who mod pool-length) + 2
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
;; LINKS
;;

to share-models
  let S-variants-1 [ get-data "S-variants" ] of end1
  let S-variants-2 [ get-data "S-variants" ] of end2
  if (is-list? S-variants-1) and (is-list? S-variants-2) and (length S-variants-1 > 0) and (length S-variants-2 > 0) [
    ;; Both turtles have an S, so they can swap their best ones!

    let best-variant-1 first S-variants-1
    let best-variant-2 first S-variants-2

    ;; Doing these redundancy checks isn't great (from a performance perspective) but it's better than filling up the agent memory with a bunch of duplicates.

    let redundant-1 false
    let redundant-2 false

    ;; is best-variant-2 already in agent 1's list?
    foreach S-variants-1 [ S1 ->
      if is-switching-matrix-equal? S1 best-variant-2 [ set redundant-1 true ]
    ]

    ;; is best-variant-1 already in agent 2's list?
    foreach S-variants-2 [ S2 ->
      if is-switching-matrix-equal? S2 best-variant-1 [ set redundant-2 true ]
    ]

    if not redundant-1 [
      ask end1 [
        ;; make sure not to overflow the variants list
        if length S-variants-1 = max-variants [
          set S-variants-1 remove-item (one-of range max-variants) S-variants-1
        ]
        set-data "S-variants" lput best-variant-2 S-variants-1
      ]
    ]

    if not redundant-2 [
      ask end2 [
        ;; make sure not to overflow the variants list
        if length S-variants-2 = max-variants [
          set S-variants-2 remove-item (one-of range max-variants) S-variants-2
        ]
        set-data "S-variants" lput best-variant-1 S-variants-2
      ]
    ]
  ]
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
  report (map [[u v] -> u + v] uu vv)
end

to-report vector-diff [uu vv]
  report (map [[u v] -> u - v] uu vv)
end

to-report vector-sumsquares [vv]
  let sumsquares 0
  foreach range length vv [ i ->
    let vvi item i vv
    set sumsquares sumsquares + (vvi * vvi)
  ]
  report sumsquares
end

to-report vector-scale [vv c]
  let xx []
  foreach range length vv [ i ->
    let vvi item i vv
    set xx lput (vvi * c) xx
  ]
  report xx
end

to-report vector-multiply-by-item [uu vv]
  report (map [[u v] -> u * v] uu vv)
end


;; reports in index in the weights list
to-report weighted-choice [ weights ]
  if sum weights = 0 [ report one-of range length weights ]

  let ww normalize-probability-vector weights
  let pp random-float 1

  let cumulative 0
  foreach range (length weights - 1) [ i ->
    set cumulative cumulative + (item i weights)
    if pp < cumulative [ report i ]
  ]
  report (length weights - 1)
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

to-report strategy-weighted-random [ weight0 weight1 weight2 ]
  report weighted-choice (list weight0 weight1 weight2)
end

to-report strategy-slow-random [ period ]
  if ticks = 0 [ set-data "phase" random period ]
  let phase get-data "phase"
  ifelse ticks mod period = phase
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

to-report strategy-check-last-t-rounds [ t ]
  ;; pick the pool with the lowest total population summed over the last t rounds
  ;; note: if there's a tie, it will choose the lower risk round. (What if that was random?)
  if ticks = 0 [ report random-pool ]

  set t min list t length agent-history

  let total-populations reduce vector-add sublist agent-history 0 t
  report position (min total-populations) total-populations
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

;; normalize so the vector adds to 1
to-report normalize-probability-vector [v]
  let total sum v
  if (total = 1) [ report v ] ;; save some computation time
  let normalized-v map [ x -> x / total ] v
  report normalized-v
end

to-report random-normalized-vector [ dim ]
  let partitions sort n-values (dim - 1) [ random-float 1 ]
  set partitions lput 1 partitions
  let v []
  let prev 0
  foreach partitions [ i ->
    set v lput (i - prev) v
    set prev i
  ]
  report v
end

to-report init-switching-probabilities-random
  report n-values 3 [ n-values 3 [ random-float 1 ] ]
end

to-report init-switching-probabilities-equal
  report n-values 3 [ n-values 3 [ 1 / 9] ]
end

to-report init-switching-probabilities-optimistic
  ;; Switching matrix with a high probability to go/stay in stable, and a low probability for the others.
  let Plarge 0.8
  let Psmall 0.1
  report n-values 3 [ i ->
    n-values 3 [ j ->
      ifelse-value (j = 0) [ Plarge ] [ Psmall ]
    ]
  ]
end

to-report init-switching-probabilities-pessimistic
  ;; Switching matrix with a high probability to go to high/low (greedy/risky)
  let Plarge 0.4
  let Psmall 0.2
  report n-values 3 [ i ->
    n-values 3 [ j ->
      ifelse-value (j = 0) [ Psmall ] [ Plarge ]
    ]
  ]
end



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

    if who mod 4 = 0 [
      set S init-switching-probabilities-random
    ]
    if who mod 4 = 1 [
      set S init-switching-probabilities-equal
    ]


    if who mod 3 = 0 [
      set S init-switching-probabilities-optimistic
    ]
    if who mod 3 = 1 [
      set S init-switching-probabilities-pessimistic
    ]
    if who mod 3 = 2 [
      set S init-switching-probabilities-random
    ]

    set S init-switching-probabilities-equal

    let matrix-label reduce word (sentence "matrix" n-values 5 [ random 10 ])
    set S lput matrix-label S

    set S mutate-switching-probabilities S 0.2
    set S lput matrix-label (but-last S)



    ;; normalize the columns (so that probabilities to 1)
    foreach [0 1 2] [ col-num ->
      let column item col-num S
      let total sum column
      let normalized-column normalize-probability-vector column
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
  if magnitude = 0 [ report S ]
  let S-mutated []

  foreach [0 1 2] [col-num ->
    let column item col-num S

    let m0 clamp (((random-float 1) * 2 - 1) * magnitude) 0 1
    let m1 clamp (((random-float 1) * 2 - 1) * magnitude) 0 1
    let m2 0 - m0 - m1

    let mutated-column vector-add column (list m0 m1 m2)

    ;; make sure they're positive and normalized
    set mutated-column map [i -> clamp i 0 1] mutated-column
    set mutated-column normalize-probability-vector mutated-column

    set S-mutated lput mutated-column S-mutated
  ]

  ;; label for this matrix shows that it's a child of the previous matrix
  let matrix-label reduce word (sentence (last S) "_" n-values 3 [random 9])
  set S-mutated lput matrix-label S-mutated

  report S-mutated
end

to-report get-best-switching-variant
  let S-variants get-data "S-variants"
  let S-errors get-data "S-errors"
  if S-variants = 0 [ report 0 ] ;; no S matrices exist

  let min-error min S-errors
  let min-position position min-error S-errors
  report item min-position S-variants
end


to-report get-prediction-error [S turns-ago]
  if (turns-ago > (length agent-history - 2)) [ report 0 ]
  let agent-counts-0 item (turns-ago + 1) agent-history
  let agent-counts-1 item turns-ago agent-history
  let predicted-counts predict-counts S agent-counts-0
  let prediction-error vector-sumsquares (vector-diff predicted-counts agent-counts-1)
  report prediction-error
end

to-report predict-counts [S counts]
  report matrix-dot-vector (but-last S) counts
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
  let S but-last get-switching-probabilities ["S"] ;; use but-last, because the S contains a label at the end

  if ticks = 0 [ report random-pool ]

  let agents-previous-turn first agent-history
  let agent-prediction matrix-dot-vector S agents-previous-turn
  let payoff-prediction calculate-my-possible-payoffs agent-prediction

  ;; let's go to the pool with highest predicted payoff
  let max-payoff max payoff-prediction
  report position max-payoff payoff-prediction
end

;; Same as predictive-strategy-1 but keeps a bunch of options in its back pocket.
to-report strategy-switching-matrix-1
  ;;let num-variants 10
  let S-variants []

  ifelse ticks = 0 [
    foreach range num-variants [
      set S-variants lput (get-switching-probabilities [""]) S-variants
    ]
    set-data "S-variants" S-variants
    set-data "S" first S-variants
    set-data "S-choices" []
  ][
    set S-variants get-data "S-variants"
  ]

  if ticks = 0 [ report random-pool ]
  if ticks = 1 [ report predictive-strategy-1 ] ;; this will use the first S matrix and also set it

  let S-with-min-error get-data "S"
  let min-error 9999999

  ;; Check previous m rounds, and retroactively see which S matrix would have been best
  ;;let m 5
  let mm m
  set mm clamp m 0 length agent-history
  let agents-in-recent-turns sublist agent-history 0 mm

  ;; see what the max payoff would have been

  let S-errors []


  foreach S-variants [ S ->

    ;; simulate prediction with this S
    let prediction-error 0
    foreach agents-in-recent-turns [ agent-counts ->
      let actual-payoffs calculate-my-possible-payoffs agent-counts
      let actual-max-payoff max actual-payoffs
      let agents-predicted matrix-dot-vector (but-last S) agent-counts
      let payoff-prediction calculate-my-possible-payoffs agents-predicted

      set prediction-error prediction-error + vector-sumsquares (vector-diff agents-predicted agent-counts)
    ]

    set S-errors lput prediction-error S-errors

    if prediction-error < min-error [
      set min-error prediction-error
      set S-with-min-error S
    ]
    ;; ...

    ;;let predicted-max-payoff max payoff-prediction
    ;;let chosen-position position predicted-max-payoff payoff-prediction

    ;; if this S gave us the max payoff, then mark it as a good one to be used
    ;;if predicted-max-payoff = actual-max-payoff [
    ;;  set S-best-choices lput S S-best-choices
    ;;]
  ]

  ;; sort the variants best > worst
  let sorted-positions sort-by [[i j] ->
    (item i S-errors) < (item j S-errors)
  ] range length S-variants

  let sorted-S-variants map [i -> item i S-variants] sorted-positions
  let sorted-S-errors map [i -> item i S-errors] sorted-positions


  set-data "S-variants" sorted-S-variants
  set-data "S-errors" sorted-S-errors
  set-data "error" min-error
  set-data "S" S-with-min-error
  let S S-with-min-error

  ;;if length S-best-choices > 0 [
  ;;  set S one-of S-best-choices
  ;;  set-data "S" S
  ;;]

  ;; now use this S matrix for our next choice
  let S-choices get-data "S-choices"
  set-data "S-choices" fput (last S) S-choices ;; only record labels

  let agents-previous-turn first agent-history
  let agent-prediction matrix-dot-vector (but-last S) agents-previous-turn
  let payoff-prediction calculate-my-possible-payoffs agent-prediction

  ;; let's go to the pool with highest predicted payoff
  let max-payoff max payoff-prediction
  let pools-with-max-payoff filter [ i -> (item i payoff-prediction) = max-payoff] (range length payoff-prediction)
  report one-of pools-with-max-payoff
  ;;report position max-payoff payoff-prediction
end

;; Same as predictive-strategy-1 but with some stochasticity.
to-report strategy-switching-matrix-1-stochastic [random-probability]
  if ticks <= 2 [ report strategy-switching-matrix-1 ]

  ifelse (random-float 1) < random-probability [
    report random-pool
  ] [
    report strategy-switching-matrix-1
  ]
end


;; Same as predictive-strategy-1 but does some genetic variance of the strategies in its list
to-report strategy-switching-matrix-2
  if ticks < 2 [ report strategy-switching-matrix-1 ] ;; don't start varying S choices until tick 2

  let choice strategy-switching-matrix-1

  ;; replace some S choices with variants of my latest used S
  let S-variants get-data "S-variants"
  let S get-data "S"
  let num-replacements floor num-variants * percent-to-replace
  let indices-to-replace (range (length S-variants - num-replacements) (length S-variants))
  ;;let indices-to-replace n-of num-replacements range num-variants ;; Random indices

  foreach indices-to-replace [ i ->
    let mutated mutate-switching-probabilities S mutation-magnitude-sp
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


to-report strategy-weighted-memory [memory-size]
  let weights []
  ifelse ticks = 0 [
    ;; initialize weights
    set weights normalize-probability-vector (n-values memory-size [random-float 1])
    set-data "weights" weights
  ][
    set weights get-data "weights"
  ]

  if ticks < memory-size [
    report random-pool
  ]

  ;; get the recent rounds
  let recent-history sublist agent-history 0 memory-size
  let weighted-history (map vector-scale recent-history weights)
  let agent-prediction reduce vector-add weighted-history


  let payoff-prediction calculate-my-possible-payoffs agent-prediction
  let max-payoff max payoff-prediction
  report position max-payoff payoff-prediction
end

to-report strategy-a
  report 0
end

to-report strategy-risk-reduction
  if ticks = 0 [ report 0 ]

  let my-current-wealth wealth
  let all-wealths [wealth] of turtles
  let percentile-50 median all-wealths

  let higher-wealths [wealth] of turtles with [wealth > percentile-50]

  let percentile-75 percentile-50
  if length higher-wealths > 0 [
    set percentile-75 median higher-wealths
  ]

  ;; The risk I am willing to take is dependent on how my wealth compares to others.
  if my-current-wealth < percentile-50 [ report 0 ]
  if my-current-wealth < percentile-75 [ report 1 ]
  report 2;
end

;;;;;;;;;;;

;; A strategy with a lot of parameters!
;; b + x + r + h should be normalized to 1.
;; b: the portion of the decision given to a biased choice (favoring one pool over another)
;; x: the portion of the decision given to a random choice
;; r: the portion of the decision based on considering past rewards
;; h: the portion of the decision based on considering past history (populations of agents)
;; b0, b1, b2: if we decided purely based on bias, then these are the weights given to each pool (sum to 1)
;;
;; component weights is: [b x r h]
;; bias weights is [b0 b1 b2]
to-report strategy-parameterized [component-weights bias-weights]
  ;; Decide on a pool choice for each component
  let poolB strategy-weighted-random (item 0 bias-weights) (item 1 bias-weights) (item 2 bias-weights)
  let poolX strategy-random
  let poolR strategy-risk-reduction
  let poolH strategy-weighted-memory 2 ;; strategy-check-last-t-rounds 20

  let choices (list poolB poolX poolR poolH)

  ;; normalize all the probabilities
  let ww normalize-probability-vector component-weights

  ;; choose a pool!
  let choice-index weighted-choice ww
  report item choice-index choices
end


;;;;;;;;;;;;

;; Tournament!

to-report choose-strategy-9 [ low-payoff high-payoff low-number high-number my-payoffs my-choices ]
  let the-pool 0
  report the-pool

end
@#$#@#$#@
GRAPHICS-WINDOW
15
420
4628
669
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
306
0
15
1
1
1
ticks
30.0

SLIDER
15
20
187
53
tau
tau
0
5
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
15
55
187
88
N
N
1
100
50.0
5
1
NIL
HORIZONTAL

BUTTON
15
95
70
128
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
75
95
130
128
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
135
95
190
128
Go 1
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
920
275
1140
395
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
528
105
863
270
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
360
30
484
95
Total Wealth
total-wealth
1
1
16

PLOT
440
285
643
405
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
230
285
431
405
Wealth by strategy
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ";; hacky to get this to be per-strategy instead of per-turtle.\nif ticks = 0 [\n  ask turtles [\n    create-temporary-plot-pen (word strategy)\n  ]\n]\nask turtles [\n  let s strategy\n  let s-turtles sort [who] of turtles with [strategy = s]\n  if first s-turtles = who [\n    set-current-plot-pen (word strategy)\n    set-plot-pen-color item strategy strategy-colors\n    plot wealth-per-turtle-with-strategy strategy\n  ]\n]"
PENS

BUTTON
15
135
185
168
Setup & Go 100 ticks
setup\nrepeat 100 [ go ]\nfinish-game\n\n;;let strategies remove-duplicates [strategy] of turtles\n;;show map [i -> list (item i strategies) precision ((mean [wealth] of turtles with [strategy = (item i strategies)]) / ticks) 3 ] range length strategies
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
1515
10
1585
43
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
230
30
354
95
Mean wealth
mean [wealth] of turtles
2
1
16

SLIDER
1180
145
1352
178
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
230
105
520
271
Individual agents wealth
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"ask turtles [\ncreate-temporary-plot-pen (word who)\n]" "ask turtles [\n    set-current-plot-pen (word who)\n    set-plot-pen-color item strategy strategy-colors\n    plot wealth\n  ]"
PENS
"default" 1.0 0 -16777216 true "" ""

SLIDER
1493
145
1665
178
num-variants
num-variants
1
20
0.0
1
1
NIL
HORIZONTAL

PLOT
1180
285
1455
405
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
1477
137
1652
170
percent-to-replace
percent-to-replace
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
930
87
1134
120
mutation-magnitude
mutation-magnitude
0
0.5
0.0
0.05
1
NIL
HORIZONTAL

PLOT
650
285
862
405
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

SLIDER
1193
40
1365
73
mem-size
mem-size
0
10
0.0
1
1
NIL
HORIZONTAL

CHOOSER
1200
210
1292
255
strategy1
strategy1
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
0

CHOOSER
1190
230
1282
275
strategy2
strategy2
0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20
0

TEXTBOX
1476
110
1716
128
Params for SwitchingMatrix strategies\n
12
0.0
1

TEXTBOX
1196
18
1447
36
Params for WeightedMemory strategies
12
0.0
1

SLIDER
1472
247
1644
280
the-b
the-b
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
1480
261
1652
294
the-r
the-r
0
1
0.0
0.1
1
NIL
HORIZONTAL

SLIDER
1491
271
1663
304
the-h
the-h
0
1
0.0
0.1
1
NIL
HORIZONTAL

MONITOR
1504
281
1561
326
the-x
1 - the-h - the-b - the-r
17
1
11

MONITOR
490
30
587
95
gini index
gini-index [wealth] of turtles
3
1
16

SLIDER
932
49
1146
82
num-param-pop-mutations
num-param-pop-mutations
1
10
0.0
1
1
NIL
HORIZONTAL

TEXTBOX
936
24
1131
43
Population GA parameters
12
0.0
1

SLIDER
934
134
1135
167
mutation-probability
mutation-probability
0
1
0.0
0.05
1
NIL
HORIZONTAL

SLIDER
928
178
1149
211
contraction-magnitude
contraction-magnitude
0
1
0.0
0.05
1
NIL
HORIZONTAL

SLIDER
942
186
1161
219
contraction-probability
contraction-probability
0
1
0.0
0.1
1
NIL
HORIZONTAL

BUTTON
15
330
115
363
CLEAR ALL
clear-output\nclear-all\nrandom-seed 2345
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
15
290
115
323
Run Ng Gens
repeat Ng * num-param-pop-mutations [\n  setup\n  repeat 100 [ go ]\n  finish-game\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
130
290
199
350
Ng
0.0
1
0
Number

SLIDER
1467
124
1692
157
mutation-magnitude-sp
mutation-magnitude-sp
0
1
0.0
0.01
1
NIL
HORIZONTAL

SWITCH
15
180
203
213
use-meta-ga?
use-meta-ga?
1
1
-1000

BUTTON
1509
156
1721
189
ARGH NOT A SWEET SPOT
set percent-to-replace 1\nset num-variants 1\nset mutation-magnitude-sp 0.5
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
1518
175
1712
208
Default to one static S
set percent-to-replace 0\nset num-variants 1\nset mutation-magnitude-sp 0
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
1469
416
1608
449
use-sharing?
use-sharing?
1
1
-1000

BUTTON
1463
347
1713
380
print all turtle S-variants to console
ask turtles [ show map last get-data \"S-variants\" ]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1533
190
1705
223
max-variants
max-variants
0
100
0.0
1
1
NIL
HORIZONTAL

SWITCH
1474
460
1610
493
use-groups?
use-groups?
1
1
-1000

SLIDER
1457
505
1629
538
num-groups
num-groups
0
100
0.0
1
1
NIL
HORIZONTAL

BUTTON
1470
371
1618
404
Matrices of turtle 0
ask turtle 0 [\nlet sv get-data \"S-variants\"\nforeach sv [s ->\n  print last s\n  let ss (map [i -> nested-precision i 2] but-last s)\n  foreach ss print\n  print \"\"\n]\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1478
50
1650
83
m
m
0
50
0.0
1
1
NIL
HORIZONTAL

SWITCH
15
215
205
248
print-ga-to-console?
print-ga-to-console?
1
1
-1000

SLIDER
939
227
1111
260
keep-mutations
keep-mutations
0
20
0.0
1
1
NIL
HORIZONTAL

MONITOR
695
25
862
90
NIL
generation-count
17
1
16

INPUTBOX
1335
195
1450
268
the-seed
0.0
1
0
Number

SWITCH
15
250
205
283
use-gini-in-fitness?
use-gini-in-fitness?
0
1
-1000

BUTTON
15
370
115
403
Assign pop
assign-params-to-turtles pop
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
121
370
206
403
Clear pop
set pop 0
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
1185
85
1429
118
Show ORDERED turtle params
show map [ i -> [ get-data \"params\" ] of turtle i ] (range count turtles)
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
620
10
837
43
use-tournament-turtle
use-tournament-turtle
1
1
-1000

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
  <experiment name="vary tau, measure pool choices" repetitions="8" runMetricsEveryStep="true">
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
    <steppedValueSet variable="tau" first="0" step="0.025" last="1"/>
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
  <experiment name="vary tau, measure agent variance" repetitions="8" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>total-wealth</metric>
    <metric>get-for-pool recent-agent-history 0</metric>
    <metric>get-for-pool recent-agent-history 1</metric>
    <metric>get-for-pool recent-agent-history 2</metric>
    <metric>agent-variance</metric>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <steppedValueSet variable="tau" first="0" step="0.025" last="1"/>
    <enumeratedValueSet variable="p">
      <value value="0"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Pool counts of each turtle" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>[list strategy total-pool-counts] of turtles</metric>
    <enumeratedValueSet variable="percent-to-replace">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-magnitude">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau">
      <value value="0.63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem-size">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-variants">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Switch counts of each turtle" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="100"/>
    <metric>[list strategy (flatten-2d-array switch-counts) ] of turtles</metric>
    <enumeratedValueSet variable="percent-to-replace">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-magnitude">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau">
      <value value="0.63"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem-size">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-variants">
      <value value="1"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GA" repetitions="1" runMetricsEveryStep="false">
    <setup>clear-all
random-seed the-seed</setup>
    <go>repeat Ng * num-param-pop-mutations [
  setup
  repeat 100 [ go ]
  finish-game
]</go>
    <timeLimit steps="1"/>
    <metric>mutation-history</metric>
    <enumeratedValueSet variable="use-gini-in-fitness?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="10"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau">
      <value value="0"/>
      <value value="0.33"/>
      <value value="0.67"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="keep-mutations">
      <value value="0"/>
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="the-seed">
      <value value="2345"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-to-replace">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-variants">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-magnitude-sp">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem-size">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-probability">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-magnitude">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-param-pop-mutations">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ng">
      <value value="200"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="GA - single run" repetitions="1" runMetricsEveryStep="false">
    <setup>clear-all
random-seed the-seed</setup>
    <go>repeat Ng * num-param-pop-mutations [
  setup
  repeat 100 [ go ]
  finish-game
]</go>
    <timeLimit steps="1"/>
    <metric>mutation-history</metric>
    <enumeratedValueSet variable="use-gini-in-fitness?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="N">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tau">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="keep-mutations">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="the-seed">
      <value value="2345"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="percent-to-replace">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-variants">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-magnitude-sp">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="p">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mem-size">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-probability">
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mutation-magnitude">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-param-pop-mutations">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Ng">
      <value value="200"/>
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
1
@#$#@#$#@
