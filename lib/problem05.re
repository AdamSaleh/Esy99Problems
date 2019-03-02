
// 00 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy)
let rec last = (lst) => 
  switch (lst) {
  | [] => None
  | [a] => Some(a)
  | [a, ...rest] => last(rest)
  }
  
let%test _ = last([]) == None
let%test _ = last([1]) == Some(1)
let%test _ = last([1, 2, 3]) == Some(3)

// 2. Find the last but one (last and penultimate) elements of a list. (easy)
let rec lasttwo = (lst) => 
  switch (lst) {
  | [] => None
  | [a] => None
  | [a, b] => Some((a, b))
  | [_, ...rest] => lasttwo(rest)
  }

let%test _ = lasttwo([]) == None
let%test _ = lasttwo([1]) == None
let%test _ = lasttwo([1, 2, 3]) == Some((2,3))

// 3. Find the k'th element of a list. (easy)
let rec at = (k, lst) => 
  switch (lst) {
  | [] => None
  | [a, ...rest] when k == 1 => Some(a)
  | [_, ...rest] when k > 1 => at(k-1, rest)
  | _ => None 
  }

let%test _ = at(1, []) == None
let%test _ = at(-1, [1]) == None
let%test _ = at(1,[1, 2, 3]) == Some(1)
let%test _ = at(3,[1, 2, 3]) == Some(3)
let%test _ = at(4,[1, 2, 3]) == None

// 4. Find the number of elements of a list. (easy)
let rec len = (lst) => 
  switch(lst) {
  | [] => 0
  | [_, ...rest] => 1+len(rest)
  }

let%test _ = len([]) == 0
let%test _ = len([1,2,3,4]) == 4

// 5. Reverse a list. (easy)
let rec reverse = (lst) => 
  switch(lst) {
  | [] => []
  | [a] => [a]
  | [a, ...rest] => List.concat([reverse(rest),[a]])
  }

let%test _ = reverse([]) == []
let%test _ = reverse([1,2,3,4]) == [4,3,2,1]