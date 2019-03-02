// 6. Find out whether a list is a palindrome. (easy)
let is_palindrome = (lst) => lst == List.rev(lst)

let%test _ = is_palindrome([])
let%test _ = is_palindrome([1])
let%test _ = is_palindrome([1,2,1])
let%test _ = is_palindrome([1,2,2,1])
let%test _ = is_palindrome([1,2])==false

// 7. Flatten a nested list structure. (medium)
