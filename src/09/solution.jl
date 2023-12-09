using Polynomials

const r_number = r"(-?\d+)"

function step(numbers::Vector{Int})::Vector{Int}
    return map(
        i -> numbers[i+1] - numbers[i],
        1:(length(numbers)-1)
    )
end

function extrapolate(line::String, left::Bool)::Int
    ys = map(
        m -> parse(Int, m.match),
        eachmatch(r_number, line)
    )
    xs = 1:(length(ys))
    return round(fit(xs, ys)(left ? 0 : length(xs) + 1))
end

function mirage_maintenance(lines::Vector{String}, pt2::Bool)::Int
    return mapreduce(
        line -> extrapolate(line, pt2),
        +,
        lines
    )
end

# pt1
@time println("Part 1: $(mirage_maintenance(readlines("src/09/input.txt"), false))")

# pt2
@time println("Part 2: $(mirage_maintenance(readlines("src/09/input.txt"), true))")
