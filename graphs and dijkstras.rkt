#lang racket

; Practices making a data structure within a functional programming language and using said data 
; structure to solve a problem implementing an algorithm in a functional way.

(define (make-graph)
  ; make a list that will contain the vertices in the graph
  (define vertices '())
  ; make a list that will contain the edges in list form containing the vertices and weight
  (define edges '())
  (define (dispatch method-name)
    (cond ((eq? method-name 'add-vertex!) add-vertex!)
          ((eq? method-name 'get-vertices) get-vertices)
          ((eq? method-name 'add-edge!) add-edge!)
          ((eq? method-name 'get-weight) get-weight)
          ((eq? method-name 'get-neighbors) get-neighbors)
          (#t (error "Bad method name"))))
  ; add vertex to graph
  (define (add-vertex! v)
    (set! vertices (cons v vertices)))
  (define (get-vertices)
    vertices)
  ; adds edge to graph with weight
  (define (add-edge! v1 v2 w)
    (set! edges (cons (list v1 v2 w) edges)))
  ; returns weight of edge if it exists
  (define (get-weight v1 v2)
    (define (weight-helper v1 v2 edges)
      (cond
        ((null? edges) #f)
        ; if v1 and v2 in any order are equal to any edge in the list of edges return the weight.
        ((and (equal? (car (car edges)) v1) (equal? (cadr (car edges)) v2)) (caddr (car edges)))
        ((and (equal? (car (car edges)) v2) (equal? (cadr (car edges)) v1)) (caddr (car edges)))
        (else (weight-helper v1 v2 (cdr edges)))))
    (weight-helper v1 v2 edges))
  ; returns the list of neighbors of vertex v
  (define (get-neighbors v)
    (define (neighbor-helper edges neighbors)
      (cond
        ((null? edges) neighbors)
        ((equal? (caar edges) v)
         (neighbor-helper (cdr edges) (cons (cadar edges) neighbors)))
        ((equal? (cadar edges) v)
         (neighbor-helper (cdr edges)
                               (cons (caar edges) neighbors)))
        (#t (neighbor-helper (cdr edges) neighbors))))
    (neighbor-helper edges '()))
  dispatch)

(define (dijkstra graph start finish)
  ; A "path" is a list of vertices along with the sum of the weights on the edges
  ; making up the path.
  (define (make-path vertices distance)
    (cons vertices distance))
  ; Initialize "frontier" to a list containing just the degenerate path starting
  ; at start, with sum of distances for this path = 0
  (define frontier (list (make-path (list start) 0)))
  ; Keep track of explored vertices to prevent cycles
  (define explored '())
  
  ; Helper function that acts as while loop and takes the frontier and explored lists as arguments
  (define (dijkstra-helper frontier explored)
    (cond ((null? frontier) #f)
          (#t 
           (let ((curpath (car frontier)))
             (let ((curvertex (car curpath)))
               (if (equal? (car curvertex) finish)
                   curpath
                   (if (member curvertex explored)
                       ; If we've already explored this vertex, skip it
                       (dijkstra-helper (cdr frontier) explored)
                       ; Otherwise, explore its neighbors
                       (let* ((neighbors ((graph 'get-neighbors) (car curvertex)))
                              (new-paths (map (lambda (neighbor)
                                                (let* ((weight ((graph 'get-weight) (car curvertex) neighbor))
                                                       (new-distance (+ (cdr curpath) weight)))
                                                  (make-path (cons neighbor (car curpath)) new-distance)))
                                              neighbors))
                              ; Filter out paths with cycles or that end with a vertex in the explored list
                              (filtered-paths (filter (lambda (path)
                                                         (let ((vertices (cdar path)))
                                                           (not (member (car vertices) explored))))
                                                      new-paths))
                              ; Add the filtered paths to the frontier
                              (new-frontier (append (cdr frontier) filtered-paths)))
                         ; Sort the frontier by smallest distance
                         (set! new-frontier (sort new-frontier (lambda (path1 path2)
                                                                 (< (cdr path1) (cdr path2)))))
                       ; Add the current vertex to the explored list
                       (set! explored (cons curvertex explored))
                       ; Recursively call dijkstra-helper with updated frontier and explored lists
                       (dijkstra-helper new-frontier explored)))))))))
  ; Call the helper function with initial frontier and explored lists
  (let ((result (dijkstra-helper frontier explored)))
    (if result
        ; If a path was found, return it in the correct order
        (make-path (reverse (car result)) (cdr result))
        ; Otherwise, return #f
        #f)))