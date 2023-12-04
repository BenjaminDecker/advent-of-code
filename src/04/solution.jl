r_number = r"(\d+)"

function get_winning_numbers(line::String)::Set{Int}
    winning_numbers_substr = line[(findfirst(':', line)+1):(findfirst('|', line)-1)]
    return reduce(
        (acc, winning_number_match) -> push!(acc, parse(Int, winning_number_match.match)),
        eachmatch(r_number, winning_numbers_substr);
        init=Set{Int}()
    )
end

function get_picked_numbers(line::String)::Set{Int}
    picked_numbers_substr = line[(findfirst('|', line)+1):end]
    return reduce(
        (acc, picked_number_match) -> push!(acc, parse(Int, picked_number_match.match)),
        eachmatch(r_number, picked_numbers_substr);
        init=Set{Int}()
    )
end

get_matches(line::String)::Int = length(intersect(get_winning_numbers(line), get_picked_numbers(line)))

function get_score(line::String)::Int
    matches = get_matches(line)
    if matches == 0
        return 0
    end
    return 1 << (matches - 1)
end

scratchcards_pt1(lines::Vector{String})::Int = mapreduce(get_score, +, lines)

function scratchcards_pt2(lines::Vector{String})::Int
    num_scratchcards = fill(1, length(lines))
    matches_per_line = map(get_matches, lines)
    for i in eachindex(matches_per_line)
        num_scratchcards[(i+1):(i+matches_per_line[i])] .+= num_scratchcards[i]
    end
    return reduce(+, num_scratchcards)
end

# pt1
println("Part 1: $(scratchcards_pt1(readlines("src/04/input.txt")))")

# pt2
println("Part 2: $(scratchcards_pt2(readlines("src/04/input.txt")))")