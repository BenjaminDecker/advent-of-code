struct Colorinfo
    max::Int
    regex::Regex
end

const red = Colorinfo(12, r"(\d+) red")
const green = Colorinfo(13, r"(\d+) green")
const blue = Colorinfo(14, r"(\d+) blue")
const colors = [red, green, blue]

const r_id = r"Game (\d+)"

function get_color_amount(split::AbstractString, color::Colorinfo)::Int
    m = match(color.regex, split)
    if m === nothing
        return 0
    end
    return parse(Int, m[1])
end

is_split_possible(split::AbstractString, color::Colorinfo)::Bool = get_color_amount(split, color) <= color.max

function is_line_possible(line::String)::Bool
    return mapreduce(
        split -> mapreduce(color -> is_split_possible(split, color), &, colors),
        &,
        eachsplit(line, ';')
    )
end

get_id_if_possible_else_0(line::String)::Int = is_line_possible(line) ? parse(Int, match(r_id, line)[1]) : 0

function get_minimum_amount_necessary(line::String, color::Colorinfo)::Int
    return mapreduce(
        split -> get_color_amount(split, color),
        max,
        eachsplit(line, ';')
    )
end

function get_power_of_line(line::String)::Int
    return mapreduce(
        color -> get_minimum_amount_necessary(line, color),
        *,
        colors
    )
end

function cube_conundrum(lines::Vector{String}, part2::Bool)::Int
    fun = part2 ? get_power_of_line : get_id_if_possible_else_0
    return mapreduce(
        line -> fun(line),
        +,
        lines
    )
end

# pt1
println("Part 1: $(cube_conundrum(readlines("src/02/input.txt"), false))")

# pt2
println("Part 2: $(cube_conundrum(readlines("src/02/input.txt"), true))")