;;;;            Generic sequence operations

;;; There are many kinds of data that can be used to represent sequences: 
;;;     examples include strings, lists, and vectors.

;;; There are operations that can be defined for all sequence types.

;;;                    Constructing
;;;
;;; (sequence:construct <sequence-type> <item-1> ... <item-n>)
;;;    Constructs a new sequence of the given type and of size n with
;;;    the given elements: item-1 ... item-n

;;; (sequence:null <sequence-type>)
;;;    Produces the null sequence of the given type


;;;                     Selecting
;;;
;;; (sequence:ref <sequence> <i>)
;;;    Returns the ith element of the sequence.  We use zero-based
;;;    indexing, so for a sequence of length n the ith item is
;;;    referenced by (sequence:ref <sequence> <i-1>).

;;; (sequence:size <sequence>)
;;;    Returns the number of elements in the sequence.

;;; (sequence:type <sequence>)
;;;    Returns the predicate defining the type of the sequence given.


;;;                     Testing
;;;
;;; (sequence:null? <sequence>)
;;;    Returns #t if the sequence is null, otherwise returns #f.

;;; (sequence:equal? <sequence-1> <sequence-2>)
;;;    Returns #t if the sequences are of the same type and have equal
;;;    elements in the same order, otherwise returns #f.


;;;                     Mutation
;;;
;;; Some sequences are immutable, while others can be changed.  
;;;
;;; For those that can be modified we can change an element:
;;;
;;; (sequence:set! <sequence> <i> <v>) 
;;;    Sets the ith element of the sequence to v.

;;;                  Cutting and Pasting
;;;
;;;  (sequence:subsequence <sequence> <start> <end>)
;;;    The arguments start and end must be exact integers such that 
;;;       0 <= start <= end <= (sequence:size <sequence>).
;;;    Returns a new sequence of the same type as the given sequence,
;;;    of size end-start with elements selected from the given sequence.
;;;    The new sequence starts with the element of the given sequence
;;;    referenced by start.  It ends with the element of the given
;;;    sequence referenced by end-1.

;;; (sequence:append <sequence-1> ... <sequence-n>)
;;;    Requires that the sequences are all of the same type.  Returns
;;;    a new sequence of the type, formed by concatenating the
;;;    elements of the given sequences.  The size of the new sequence
;;;    is the sum of the sizes of the given sequences.

;;;                      Iterators
;;;
;;; (sequence:generate <sequence-type> <n> <function>)
;;;    Makes a new sequence of the given sequence type, of size n.
;;;    The ith element of the new sequence is the value of the 
;;;    function at the index i.

;;; (sequence:map <function> <seq-1> ... <seq-n>)
;;;    Requires that the sequences given are of the same size and
;;;    type, and that the arity of the function is n.  The ith element
;;;    of the new sequence is the value of the function applied to the
;;;    n ith elements of the given sequences.

;;; (sequence:for-each <procedure> <seq-1> ... <seq-n>)
;;;    Requires that the sequences given are of the same size and
;;;    type, and that the arity of the procedure is n.  Applies the
;;;    procedure to the n ith elements of the given sequences;
;;;    discards the value.  This is done for effect.

;;;                 Filtration and Search
;;;
;;; (sequence:filter <sequence> <predicate>)
;;;    Returns a new sequence with exactly those elements of the given
;;;    sequence for which the predicate is true (does not return #f).
;;;
;;; (sequence:get-index <sequence> <predicate>)
;;;    Returns the index of the first element of the sequence that
;;;    satisfies the predicate.  Returns #f if no element of the
;;;    sequence satisfies the predicate.
;;;
;;; (sequence:get-element <sequence> <predicate>)
;;;    Returns the first element of the sequence that satisfies the
;;;    predicate.  Returns #f if no element of the sequence satisfies
;;;    the predicate.

;;;                    Accumulation
;;;
;;; (sequence:fold-right <function> <initial> <sequence>)
;;;    Returns the result of applying the given binary function,
;;;    from the right, starting with the initial value.
;;;    For example, 
;;;      (sequence:fold-right list 'end '(a b c))
;;;           => (a (b (c end)))

;;;
;;; (sequence:fold-left <function> <initial> <sequence>)
;;;    Returns the result of applying the given binary function,
;;;    starting with the initial value, from the left.
;;;    For example, 
;;;      (sequence:fold-left list 'start '(a b c))
;;;           => (((start a) b) c)



