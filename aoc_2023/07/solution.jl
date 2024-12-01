import Base: isless

const r_hand_bet = r"(?<hand>\w+) (?<bet>\d+)"

const card_to_int_pt1 = Dict(
    '2' => 2,
    '3' => 3,
    '4' => 4,
    '5' => 5,
    '6' => 6,
    '7' => 7,
    '8' => 8,
    '9' => 9,
    'T' => 10,
    'J' => 11,
    'Q' => 12,
    'K' => 13,
    'A' => 14,
)

const card_to_int_pt2 = let
    cp = copy(card_to_int_pt1)
    cp['J'] = 1
    cp
end

const card_amounts_to_type = Dict(
    [5] => 6, # Five of a kind
    [1, 4] => 5, # Four of a kind
    [2, 3] => 4, # Full house
    [1, 1, 3] => 3, # Three of a kind
    [1, 2, 2] => 2, # Two pairs
    [1, 1, 1, 2] => 1, # One pair
    [1, 1, 1, 1, 1] => 0  # High card
)

struct Hand_and_bet
    hand::Vector{Int}
    type::Int
    bet::Int
end

function get_type(hand, part2)
    card_amounts = Dict{Int,Int}()
    cards = collect(hand)
    if part2
        num_jokers = count(card -> card == card_to_int_pt2['J'], cards)
        filter!(card -> card != card_to_int_pt2['J'], cards)
    end
    for card in cards
        amount = get(card_amounts, card, 0)
        card_amounts[card] = amount + 1
    end
    sorted_amounts_vector = sort(collect(values(card_amounts)))
    if part2
        if isempty(sorted_amounts_vector)
            sorted_amounts_vector = [5]
        else
            sorted_amounts_vector[end] += num_jokers
        end
    end
    return card_amounts_to_type[sorted_amounts_vector]
end

function Hand_and_bet(line, part2)
    hand_and_bet_match = match(r_hand_bet, line)
    bet = parse(Int, hand_and_bet_match["bet"])
    dict = part2 ? card_to_int_pt2 : card_to_int_pt1
    hand = map(c -> dict[c], collect(hand_and_bet_match["hand"]))
    type = get_type(hand, part2)
    return Hand_and_bet(hand, type, bet)
end

function isless(lhs::Hand_and_bet, rhs::Hand_and_bet)
    if lhs.type != rhs.type
        return lhs.type < rhs.type
    end
    return lhs.hand < rhs.hand
end

function camel_cards(lines, part2)
    hands_and_bets = sort(map(line -> Hand_and_bet(line, part2), lines))
    return mapreduce(
        index -> index * hands_and_bets[index].bet,
        +,
        eachindex(hands_and_bets)
    )
end

# pt1
@time println("Part 1: $(camel_cards(readlines("src/07/input.txt"), false))")

# pt2
@time println("Part 2: $(camel_cards(readlines("src/07/input.txt"), true))")
