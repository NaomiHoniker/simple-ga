(ns simple-ga.core
  "Clojure simple genetic algorithm. Usage:
    lein run [-- fitness-function]

   Champlain College
   CSI-380 Spring 2019"
  (:gen-class))

(require '[simple-ga.utils :as utils])
(require '[simple-ga.fitness-functions :as ff])

(defn mutate-genome
    "Produce a mutated genome by flipping each bit with mutation-rate probability."
    [genome mutation-rate]
    (map (fn [bit]
           (if (simple-ga.utils/coin-toss? mutation-rate)
             ;; True
             (simple-ga.utils/flip-bit bit)
             ;; False
             bit
             ))
         genome)
    )
         
(defn crossover
    "Perform single-point crossover on the two genomes.
    
    A single genome is returned chosen from the two resulting genomes with equal
    probability.
    
    See: https://en.wikipedia.org/wiki/Crossover_(genetic_algorithm)#Single-point_crossover
    "
    [genome1 genome2]
    (let [crossover-point (rand-int (count genome1))]
      (let [genome-part-1 (if (simple-ga.utils/coin-toss?)
                            ;; Heads - genome1
                            genome1
                            ;; Tails - genome2
                            genome2
                            )]
        (let [genome-part-2 (if (= genome-part-1 genome1)
                              ;; genome head is from 1, tail will be from 2
                              genome2
                              ;; genome head is from 2, tail will be from 1
                              genome1
                              )]
          (let [genome-head (subvec genome-part-1 0 crossover-point)]
            (let [genome-tail (subvec genome-part-2 crossover-point)]
              (concat genome-head genome-tail)
              )
            )
          )
        )
      )
  )
          
(defn reproduce
    "Create the next generation from the given parents using the relevant params.
    
    
    The new generation contains a total of (:population-size params) individuals
    including the unmodified parents (this is known as elitism).
    
    The remainder of the individuals are generated as follows:
        with probability (:crossover-rate params) two parents are chosen at random
        and crossed over and the resulting genome is mutated with mutation rate 
        (:mutation-rate params)
           otherwise
        with probability (- 1 (:crossover-rate params)) a single parent is chosen
        at random and its genome is mutated with mutation rate (:mutation-rate params)
    "
    [parents params]
    ;; Your code goes here
    (let [population (:population-size params)]
      (let [probability (:crossover-rate params)]
        (let [rate (:mutation-rate params)]
          (let [new-generation (concat nil parents)]
            (while (< (count new-generation) population)
              ;; While new-generation's count is less than population...
              (if (simple-ga.utils/coin-toss? probability )
                (
                 ;; If true...
                 (let [crossover-genome (apply crossover (vector (take 2 (shuffle (parents)))) )]
                   (concat new-generation (mutate-genome crossover-genome rate)))
                 ;; If false...
                 (let [single-genome (first (shuffle (parents))) ]
                   (concat new-generation (mutate-genome single-genome rate)))
                 )
                )
              )
            ))))
    )
              

(defn generate-individual
    "Generate a single individual.
    
    Each individual is a map with key :genome associated with a genome-size length
    vector of bits (0s and 1s)
    "
    [genome-size]
    {:genome (mapv (fn [_] (rand-int 2)) 
                (range genome-size))})
    
(defn generate-population
    "Generate the population.
    
    The population is a collection of (:population-size params) individuals,
    where each individual is a map with key :genome associated with a
    (:genome-size params) length vector of bits (0s and 1s)
    "
    [params]
    (let [size (:population-size params)]
      (let [length (:genome-size params)]
        (map (fn [len] (generate-individual len)) (repeat size length)) ;; Maps the gen-ind function to
        )                                                               ;; a dummy list, (size) length,
      )                                                                 ;; repeating the genome length
    )
          

(defn select-parents
    "Select and return the num-parents most fit individuals in the population.

    This is known truncation selection.
    "
    [population num-parents]
    (take num-parents (sort-by :fitness > population ) )
  )
    
(defn evaluate-individual
    "Evaluate a given individual on the specified fitness function.
    
    Returns a copy of the individual with :fitness key set to its fitness.
    
    This should evaluate the individual and set/update its fitness even if it already has a fitness value.
    "
    [individual fitness-function]
    (assoc individual :fitness (fitness-function (:genome individual))))
    
(defn evaluate-population
    "Evaluate the entire population in parallel.
    
    Returns a copy of the population with the :fitness key of each individual set to its fitness.
    
    This should evaluate all individuals in parallel and set/update their fitness (even ones that already have a fitness value).
    "
    [population fitness-function]
    ;; Your code goes here
    (vector pmap (fn [individual] (evaluate-individual individual fitness-function)) population )
    )
    

(defn run-generation
    "Run a single generation of a genetic algorithm.
    
    Performs the following steps:
        (1) evaluates the current population
        (2) selects parents
        (3) builds and returns the next generation by reproducing the parents.
    "
    [population params]
    ;; Your code goes here
    (let [runGenStep1 (evaluate-population population (:fitness-function params))] ;; Evaluate Population, pass in FF
      (let [runGenStep2 (select-parents runGenStep1 (:num-parents params))] ;; Select parents, pass in num parents
        (reproduce runGenStep2 params) ;; Reproduce parents
        ))
    )
         
(defn evolve
    "Run a genetic algorithm with the given params and return the best individual in the final generation."
    [params]
    (loop [generations-remaining (:num-generations params)
           current-population (generate-population params)]
           (if (zero? generations-remaining)
               ;; if no more generations, still need to evaluate the final population
               ;; in order to determine the best individual
               (let [evaluated-population (evaluate-population current-population (:fitness-function params))]
                    (apply max-key :fitness  evaluated-population))
               ;; otherwise move on to next generation
               (do  (println "Generation" (+ 1 (- (:num-generations params) generations-remaining)))
                    (recur (dec generations-remaining)
                           (run-generation current-population params))))))
         

(defn -main
  "Main function, run GA on max-ones problem or specified fitness function.
  
  Prints best indivdual to screen.
  "
  [& args]
  (if (> (count args) 1)
      (println "Usage:\n\tlein run [-- fitness-function]")
      (let [params {:genome-size 16
                    :population-size 100
                    :num-generations 50
                    :num-parents 5
                    :crossover-rate 0.75
                    :mutation-rate 0.1
                    :fitness-function (if (empty? args) 
                                        ff/max-ones ;; default to max-ones fitness function
                                        (resolve (symbol "simple-ga.fitness-functions" (first args))))
                   }]
       (if (:fitness-function params)
           ;; if fitness function is defined
           (println (evolve params))
           ;; otherwise invalid fitness function
           (println "unknown fitness function:" (first args)))
       (shutdown-agents)))) ;; needed in case pmap is used, otherwise program won't terminate
