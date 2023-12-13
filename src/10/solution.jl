@enum Direction north south east west

const pipe_directions = Dict{Char,Vector{Direction}}(
    'S' => [north, south, east, west],
    '|' => [north, south],
    '-' => [east, west],
    'L' => [north, east],
    'J' => [north, west],
    '7' => [south, west],
    'F' => [south, east],
    '.' => []
)

const direction_offset = Dict{Direction,CartesianIndex}(
    north => CartesianIndex(0, -1),
    south => CartesianIndex(0, 1),
    east => CartesianIndex(1, 0),
    west => CartesianIndex(-1, 0)
)

const opposite_direction = Dict{Direction,Direction}(
    north => south,
    south => north,
    east => west,
    west => east
)

function is_connected(
    coord1::CartesianIndex,
    coord2::CartesianIndex,
    pipes::Matrix{Char}
)::Bool
    coord1_directions = pipe_directions[pipes[coord1]]
    coord2_directions = pipe_directions[pipes[coord2]]
    return mapreduce(
        direction -> (
            (coord1 + direction_offset[direction] == coord2)
            &&
            (opposite_direction[direction] in coord2_directions)
        ),
        |,
        coord1_directions;
        init=false
    )
end

function get_connected_coords(
    coord::CartesianIndex,
    pipes::Matrix{Char}
)::Vector{CartesianIndex}
    return filter(
        c -> (checkbounds(Bool, pipes, c) && is_connected(c, coord, pipes)),
        [
            (coord + direction_offset[dir])
            for dir in pipe_directions[pipes[coord]]
        ]
    )
end

function next_coords(
    current_coord::CartesianIndex,
    last_coord::CartesianIndex,
    pipes::Matrix{Char}
)::CartesianIndex
    connected_coords = get_connected_coords(current_coord, pipes)
    return filter(c -> c != last_coord, connected_coords)[1]
end

function pipe_maze_pt1(lines::Vector{String})::Int
    pipes = stack(map(collect, lines))
    coord_start = findfirst(c -> c == 'S', pipes)
    current_coords = collect(get_connected_coords(coord_start, pipes))
    last_coords = Vector{CartesianIndex}([coord_start, coord_start])
    @assert length(current_coords) == 2
    path_length = 1
    while true
        path_length += 1
        for i in 1:2
            current_coord = current_coords[i]
            last_coord = last_coords[i]
            current_coords[i] = next_coords(current_coord, last_coord, pipes)
            last_coords[i] = current_coord
            if current_coords[1] == current_coords[2]
                return path_length
            end
        end
    end
end

shoelace(point1::CartesianIndex, point2::CartesianIndex)::Int =
    point1[1] * point2[2] - point1[2] * point2[1]

function shoelace(points::Vector{CartesianIndex{2}})::Int
    area_acc = shoelace(points[end], points[1])
    return abs(mapreduce(
        i -> shoelace(points[i], points[i+1]),
        +,
        1:(length(points)-1);
        init=area_acc
    ) / 2)
end

function pipe_maze_pt2(lines::Vector{String})::Int
    pipes = stack(map(collect, lines))
    coord_start = findfirst(c -> c == 'S', pipes)
    coords_chain = [next_coords(coord_start, coord_start, pipes)]
    push!(coords_chain, next_coords(coords_chain[end], coord_start, pipes))
    while coords_chain[end] != coord_start
        push!(coords_chain, next_coords(coords_chain[end], coords_chain[end-1], pipes))
    end
    return shoelace(coords_chain) - length(coords_chain) / 2 + 1
end

# pt1
@time println("Part 1: $(pipe_maze_pt1(readlines("src/10/input.txt")))")

# pt2
@time println("Part 2: $(pipe_maze_pt2(readlines("src/10/test.txt")))")
