using Memoization

const r_pattern = r"([\?\.#]+)"
const r_number = r"(\d+)"
const r_springs = r"(#+)"

@memoize Dict function count_possibilities(
    condition_record::Vector{Char},
    contiguous_groups::Vector{Int}
)::Int
    if isempty(contiguous_groups)
        if any(c -> c == '#', condition_record)
            return 0
        else
            return 1
        end
    end
    if length(condition_record) < contiguous_groups[1]
        return 0
    end
    possibilities = 0
    range = 1:(contiguous_groups[1])
    if all(c -> c != '.', condition_record[range])
        if length(condition_record) == range.stop || condition_record[range.stop+1] != '#'
            possibilities += count_possibilities(
                condition_record[(range.stop+2):length(condition_record)],
                contiguous_groups[2:length(contiguous_groups)]
            )
        end
    end
    if condition_record[1] != '#'
        possibilities += count_possibilities(
            condition_record[2:length(condition_record)],
            contiguous_groups
        )
    end
    return possibilities
end

function count_possibilities(line::String, pt2::Bool)::Int
    condition_record = collect(match(r_pattern, line).match)
    contiguous_groups = collect(map(
        m -> parse(Int, m.match),
        eachmatch(r_number, line)
    ))
    if pt2
        condition_record_appendix = vcat(['?'], condition_record)
        contiguous_groups_appendix = copy(contiguous_groups)
        for _ in 1:4
            append!(condition_record, condition_record_appendix)
            append!(contiguous_groups, contiguous_groups_appendix)
        end
    end
    return count_possibilities(condition_record, contiguous_groups)
end

function hot_springs(lines::Vector{String}, pt2::Bool)::Int
    return mapreduce(
        line -> count_possibilities(line, pt2),
        +,
        lines
    )
end

# pt1
@time println("Part 1: $(hot_springs(readlines("src/12/input.txt"), false))")

# pt2
@time println("Part 2: $(hot_springs(readlines("src/12/input.txt"), true))")
