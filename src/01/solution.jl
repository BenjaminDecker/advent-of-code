const r1 = r"\d"
const r2 = r"\d|one|two|three|four|five|six|seven|eight|nine"
const digit_dict = Dict(
    "one" => 1,
    "two" => 2,
    "three" => 3,
    "four" => 4,
    "five" => 5,
    "six" => 6,
    "seven" => 7,
    "eight" => 8,
    "nine" => 9
)

firstdigit_pt1(line::String)::Int = parse(Int, match(r1, line).match)

function firstdigit_pt2(line::String)::Int
    m = match(r2, line).match
    if haskey(digit_dict, m)
        return digit_dict[m]
    else
        return parse(Int, m)
    end
end

function trebuchet(lines::Vector{String}, part2::Bool)::Int
    fun = part2 ? firstdigit_pt2 : firstdigit_pt1
    return mapreduce(
        line -> fun(line) * 10 + fun(reverse(line)),
        +,
        lines
    )
end

# pt1
println("Part 1: $(trebuchet(readlines("src/01/input.txt"), false))")

# pt2
println("Part 2: $(trebuchet(readlines("src/01/input.txt"), true))")