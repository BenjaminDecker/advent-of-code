
function vertical_reflection(pattern::Matrix{Char}, num_smudes::Int)::Union{Int,Nothing}
    rows, cols = size(pattern)
    for col in 1:(cols-1)
        smudges = 0
        for offset in 1:(min(col, cols - col))
            smudges += count(
                row -> (pattern[row, col-offset+1] != pattern[row, col+offset]),
                1:rows
            )
        end
        if smudges == num_smudes
            return col
        end
    end
end

function point_of_incidence(pattern::Matrix{Char}, num_smudged::Int)::Int
    v_reflection = vertical_reflection(pattern, num_smudged)
    if !isnothing(v_reflection)
        return v_reflection
    end
    return 100 * vertical_reflection(permutedims(pattern, (2, 1)), num_smudged)
end

function point_of_incidence(lines::String, pt2::Bool)::Int
    return mapreduce(
        pattern -> point_of_incidence(pattern, pt2 ? 1 : 0),
        +,
        map(
            pattern -> stack(filter(!isempty, map(collect, split(pattern, '\n'))); dims=1),
            eachsplit(lines, "\n\n")
        )
    )
end

# pt1
@time println("Part 1: $(point_of_incidence(read("src/13/input.txt", String), false))")

# pt2
@time println("Part 2: $(point_of_incidence(read("src/13/input.txt", String ), true))")
