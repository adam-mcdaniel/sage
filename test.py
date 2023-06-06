
class List:
    def __init__(self, data, next=None):
        self._data = data
        self._next = next

    @staticmethod
    def cons(head, tail):
        head._next = tail
        return head
    
    def range(start, end):
        if start > end:
            return None
        else:
            return List.cons(List(start), List.range(start + 1, end))
        
    def print(self):
        print(self._data, end="")
        if self._next is not None:
            print(", ", end="")
            self._next.print()

# proc range(start: Int, end: Int): Expr {
#     if start > end {
#         return Expr of Nil;
#     } else {
#         return cons(Expr of Integer start, range(start + 1, end));
#     }
# }
x = 500
i = 0
while i < x:
    List.range(0, 500).print()
    i += 1

# for i in range(0, 500):
#     print(list(range(0, i)))