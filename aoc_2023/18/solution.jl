const r_line = r"(?<direction>[UDRL]) (?<distance>\d+) \(\#(?<color>[\da-f]+)"

const direction_offset = Dict{Char,CartesianIndex{2}}(
    'U' => CartesianIndex(0, -1),
    'D' => CartesianIndex(0, 1),
    'R' => CartesianIndex(1, 0),
    'L' => CartesianIndex(-1, 0),
    '0' => CartesianIndex(1, 0),
    '1' => CartesianIndex(0, 1),
    '2' => CartesianIndex(-1, 0),
    '3' => CartesianIndex(0, -1)
)

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

function lavaduct_lagoon_pt1(lines::Vector{String})::Int
    path = [CartesianIndex(0, 0)]
    path_length = 0
    for line in lines
        m = match(r_line, line)
        direction = m[:direction][1]
        distance = parse(Int, m[:distance])
        path_length += distance
        push!(path, path[end] + direction_offset[direction] * distance)
    end
    @assert path[end] == path[1]
    pop!(path)
    return shoelace(path) - path_length / 2 + 1 + path_length
end

function lavaduct_lagoon_pt2(lines::Vector{String})::Int
    path = [CartesianIndex(0, 0)]
    path_length = 0
    for line in lines
        m = match(r_line, line)
        color = m[:color]
        distance = parse(Int, color[1:end-1], base=16)
        direction = color[end]
        path_length += distance
        push!(path, path[end] + direction_offset[direction] * distance)
    end
    @assert path[end] == path[1]
    pop!(path)
    return shoelace(path) - path_length / 2 + 1 + path_length
end

input = readlines("src/18/input.txt")

# pt1
@time println("Part 1: $(lavaduct_lagoon_pt1(input))")

# pt2
@time println("Part 2: $(lavaduct_lagoon_pt2(input))")
