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

function number_ranges(line::String)::Vector{AbstractRange}
    return map(
        match -> (match.offset):(match.offset+length(match.match)-1),
        eachmatch(r_number, line)
    )
end

function gear_ratios_pt2(lines::Vector{String})::Int
    acc = 0
    symbol_adjacency_ranges_lines = map(symbol_adjacency_ranges, symbol_positions_lines(lines, r_symbol_gear))
    number_ranges_lines = map(number_ranges, lines)
    for linenumber in eachindex(lines)
        for gear_adjacency_range in symbol_adjacency_ranges_lines[linenumber]
            adjacent_numbers = Vector{Int}()
            for offset in -1:1
                let linenumber = linenumber + offset
                    if !(linenumber in eachindex(lines))
                        continue
                    end
                    adjacent_number_ranges = filter_overlapping(gear_adjacency_range, number_ranges_lines[linenumber])
                    append!(adjacent_numbers, map(range -> parse(Int, lines[linenumber][range]), adjacent_number_ranges))
                end
            end
            if length(adjacent_numbers) == 2
                acc += reduce(*, adjacent_numbers)
            end
        end
    end
    return acc
end

# pt1
println("Part 1: $(gear_ratios_pt1(readlines("src/03/input.txt")))")

# pt2
println("Part 2: $(gear_ratios_pt2(readlines("src/03/input.txt")))")