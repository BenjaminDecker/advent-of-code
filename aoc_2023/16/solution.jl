@enum Direction north = 1 << 0 south = 1 << 1 east = 1 << 2 west = 1 << 3

const direction_offset = Dict{Direction,CartesianIndex}(
    north => CartesianIndex(0, -1),
    south => CartesianIndex(0, 1),
    east => CartesianIndex(1, 0),
    west => CartesianIndex(-1, 0)
)

const splitter_horizontal = Dict{Direction,Set{Direction}}( # -
    north => Set{Direction}([east, west]),
    south => Set{Direction}([east, west]),
    east => Set{Direction}([east]),
    west => Set{Direction}([west])
)

const splitter_vertical = Dict{Direction,Set{Direction}}( # |
    north => Set{Direction}([north]),
    south => Set{Direction}([south]),
    east => Set{Direction}([north, south]),
    west => Set{Direction}([north, south])
)

const mirror_left = Dict{Direction,Set{Direction}}( # \
    north => Set{Direction}([west]),
    south => Set{Direction}([east]),
    east => Set{Direction}([south]),
    west => Set{Direction}([north])
)

const mirror_right = Dict{Direction,Set{Direction}}( # /
    north => Set{Direction}([east]),
    south => Set{Direction}([west]),
    east => Set{Direction}([north]),
    west => Set{Direction}([south])
)

const empty = Dict{Direction,Set{Direction}}( # .
    north => Set{Direction}([north]),
    south => Set{Direction}([south]),
    east => Set{Direction}([east]),
    west => Set{Direction}([west])
)

const char_to_dict = Dict{Char,Dict{Direction,Set{Direction}}}(
    '-' => splitter_horizontal,
    '|' => splitter_vertical,
    '\\' => mirror_left,
    '/' => mirror_right,
    '.' => empty,
)

struct Laser
    position::CartesianIndex
    direction::Direction
end

function step(laser::Laser, layout::Matrix{Char})::Set{Laser}
    new_lasers = Set{Laser}()
    for direction in char_to_dict[layout[laser.position]][laser.direction]
        new_position = laser.position + direction_offset[direction]
        if checkbounds(Bool, layout, new_position)
            push!(new_lasers, Laser(new_position, direction))
        end
    end
    return new_lasers
end

function the_floor_will_be_lava(layout::Matrix{Char}, start_laser::Laser)::Int
    working_set = Set{Laser}([start_laser])
    finished_set = Set{Laser}()
    while !isempty(working_set)
        laser = pop!(working_set)
        push!(finished_set, laser)
        new_lasers = step(laser, layout)
        union!(working_set, setdiff!(new_lasers, finished_set))
    end
    energized_tiles = Set{CartesianIndex}()
    for laser in finished_set
        push!(energized_tiles, laser.position)
    end
    return length(energized_tiles)
end

function the_floor_will_be_lava(layout::Matrix{Char})::Int
    possible_starts = Vector{Laser}()
    shape = size(layout)

    for x in 1:shape[1]
        push!(possible_starts, Laser(CartesianIndex(x, 1), south))
        push!(possible_starts, Laser(CartesianIndex(x, shape[2]), north))
    end
    for y in 1:shape[2]
        push!(possible_starts, Laser(CartesianIndex(1, y), east))
        push!(possible_starts, Laser(CartesianIndex(shape[1], y), west))
    end

    return maximum(
        map(
            start_laser -> the_floor_will_be_lava(layout, start_laser),
            possible_starts
        )
    )
end

input = eachline("src/16/input.txt") |> collect |> stack

# pt1
@time println("Part 1: $(the_floor_will_be_lava(
    input,
    Laser(CartesianIndex(1, 1), east)
    ))")

# pt2
@time println("Part 2: $(the_floor_will_be_lava(input))")
