const r_number = r"\d+"
const r_symbol = r"[^\d\.]"
const r_symbol_gear = r"\*"

function symbol_positions(line::String, symbol::Regex)::Vector{Int}
    return reduce(
        (acc, elem) -> push!(acc, elem.offset),
        eachmatch(symbol, line);
        init=Vector{Int}()
    )
end

symbol_positions_lines(lines::Vector{String}, symbol::Regex)::Vector{Vector{Int}} = map(line -> symbol_positions(line, symbol), lines)

function symbol_adjacency_ranges(symbol_positions::Vector{Int})::Vector{AbstractRange}
    return map(
        symbol_position -> (symbol_position-1):(symbol_position+1),
        symbol_positions
    )
end

filter_overlapping(range::AbstractRange, ranges::Vector{AbstractRange})::Vector{AbstractRange} = filter(other_range -> length(intersect(range, other_range)) != 0, ranges)

is_overlapping(range::AbstractRange, ranges::Vector{AbstractRange})::Bool = length(filter_overlapping(range, ranges)) == 0

function gear_ratios_pt1(lines::Vector{String})::Int
    symbol_adjacency_ranges_lines = map(symbol_adjacency_ranges, symbol_positions_lines(lines, r_symbol))
    acc = 0
    for linenumber in eachindex(lines)
        for match in eachmatch(r_number, lines[linenumber])
            number_position_range = (match.offset):(match.offset+length(match.match)-1)
            is_valid = is_overlapping(number_position_range, symbol_adjacency_ranges_lines[linenumber])
            if linenumber > 1
                is_valid &= is_overlapping(number_position_range, symbol_adjacency_ranges_lines[linenumber-1])
            end
            if linenumber < length(lines)
                is_valid &= is_overlapping(number_position_range, symbol_adjacency_ranges_lines[linenumber+1])
            end
            if !is_valid
                acc += parse(Int, match.match)
            end
        end
    end
    return acc
end

# pt1
println("Part 1: $(gear_ratios_pt1(readlines("src/03/input.txt")))")