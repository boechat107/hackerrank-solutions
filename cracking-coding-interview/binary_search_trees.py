""" Node is defined as
class node:
    def __init__(self, data):
        self.data = data
        self.left = None
        self.right = None
"""

# My first attempt was complicated, I didn't remember some properties of binary
# trees. The only advantage of this approach is that the algorithm would stop
# as soon as it detects that the tree is not binary search tree.
def recursive(root, f, visited):
    """Returns a node.data or None."""
    visited.add(root.data)
    ## Recursively check the left branch.
    leftmax = None
    left_is_bs = True
    if root.left:
        if root.left.data in visited or not (root.data > root.left.data):
            return None, False, visited
        leftmax, left_is_bs, lvset = recursive(root.left, max, visited)
        if not left_is_bs:
            return None, False, visited
        visited = visited.union(lvset)
    ## Recursively check the right branch.
    rightmin = None
    right_is_bs = True
    if root.right:
        if root.right.data in visited or not (root.data < root.right.data):
            return None, False, visited
        rightmin, right_is_bs, rvset = recursive(root.right, min, visited)
        if not right_is_bs:
            return None, False, visited
        visited = visited.union(rvset)
    ## Checking if this sub-tree is a valid binary search tree.
    if leftmax is None and rightmin is None:
        return (root.data, True, visited)
    elif leftmax is None:
        return (f(root.data, rightmin), rightmin > root.data, visited)
    elif rightmin is None:
        return (f(root.data, leftmax), leftmax < root.data, visited)
    else:
        return (f(leftmax, rightmin, root.data),
                leftmax < root.data < rightmin,
                visited)

# Much simpler approach, where the properties of binary search trees are
# considered. However, this algorithm is always O(n).

def preorder_walk(root, visited):
    if root is None:
        return visited
    visited = preorder_walk(root.left, visited)
    visited.append(root.data)
    visited = preorder_walk(root.right, visited)
    return visited

def is_sorted(xs):
    prev = xs[0]
    for i in range(1, len(xs)):
        if xs[i] <= prev:
            return False
        prev = xs[i]
    return True

def check_binary_search_tree_(root):
    if root is None:
        return False
    # _, ret, _ = recursive(root, min, set())
    #return ret
    return is_sorted(preorder_walk(root, []))
