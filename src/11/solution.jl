using Combinatorics

function get_distance(
    galaxy_pair::Vector{CartesianIndex{2}},
    empty_rows::Set{Int},
    empty_cols::Set{Int},
    expansion_size::Int
)::Int
    x1, y1 = Tuple(galaxy_pair[1])
    x2, y2 = Tuple(galaxy_pair[2])
    x_range = (x1 < x2 ? (x1:x2) : (x2:x1))
    y_range = (y1 < y2 ? (y1:y2) : (y2:y1))
    return mapreduce(
               col -> (col in x_range) ? expansion_size : 0,
               +,
               empty_cols
           ) + mapreduce(
               row -> (row in y_range) ? expansion_size : 0,
               +,
               empty_rows
           ) + length(x_range) + length(y_range) - 2
end

function cosmic_expansion(lines::Vector{String}, expansion_size::Int)::Int
    space = stack(map(collect, lines))
    empty_rows = Set{Int}()
    empty_cols = Set{Int}()

    for (index, row) in enumerate(eachrow(space))
        if all(e -> e == '.', row)
            push!(empty_cols, index)
        end
    end
    for (index, col) in enumerate(eachcol(space))
        if all(e -> e == '.', col)
            push!(empty_rows, index)
        end
    end
    galaxies = filter(
        coord -> space[coord] == '#',
        CartesianIndices(space)
    )
    return mapreduce(
        pair -> get_distance(pair, empty_rows, empty_cols, expansion_size),
        +,
        combinations(galaxies, 2)
    )
end

# pt1
@time println("Part 1: $(cosmic_expansion(readlines("src/11/input.txt"), 1))")

# pt2
@time println("Part 2: $(cosmic_expansion(readlines("src/11/input.txt"), 1_000_000-1))")
