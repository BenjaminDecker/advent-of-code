mutable struct Tree
    corrds::CartesianIndex{2}
    heat_loss::Int
    children::Vector{Tree}
    parent::Union{Tree,Nothing}
end

function clumsy_crucible(input::Matrix{Char})::Int
    start_node = CartesianIndex(1, 1)
    root = Tree(CartesianIndex(1, 1), 0, Vector{Tree}(), nothing)
    
    return 1
end

input = eachline("src/16/input.txt") |> collect |> stack

# pt1
@time println("Part 1: $(clumsy_crucible(input))")

# pt2
@time println("Part 2: $(clumsy_crucible(input))")
