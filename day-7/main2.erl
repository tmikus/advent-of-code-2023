-module(main2).
-export([start/0]).

-record(hand, {bet, cards, handValue}).

charToString(Char) -> [Char].

compareHands(Left, Right) ->
  if 
    Left#hand.handValue > Right#hand.handValue -> false;
    Left#hand.handValue < Right#hand.handValue -> true;
    true ->
      CardPairs = lists:zip(Left#hand.cards, Right#hand.cards),
      compareHandsCardByCard(CardPairs)
  end.

compareHandsCardByCard([]) -> true;
compareHandsCardByCard([{Left, Right} | Rest]) -> 
  if
    Left > Right -> false;
    Left < Right -> true;
    true -> compareHandsCardByCard(Rest)
  end.

computeHandValue(Cards) ->
  CardsWithoutJokers = lists:filter(fun(Card) -> Card > 1 end, Cards),
  JokersCount = length(Cards) - length(CardsWithoutJokers),
  CardCountMap = getHandCardCountMap(CardsWithoutJokers),
  CardCountList = getSortedHandCardCountList(CardCountMap),
  case JokersCount of
    5 -> 7; % five of a kind
    4 -> 7; % five of a kind
    3 ->
      case CardCountList of
        [{_, 2} | _] -> 7; % five of a kind
        _ -> 6            % four of a kind
      end;
    2 ->
      case CardCountList of
        [{_, 3} | _] -> 7; % five of a kind
        [{_, 2} | _] -> 6; % four of a kind
        _ -> 4            % three of a kind
      end;
    1 ->
      case CardCountList of
        [{_, 4} | _] -> 7;          % five of a kind
        [{_, 3} | _] -> 6;          % four of a kind
        [{_, 2}, {_, 2} | _] -> 5;  % full house
        [{_, 2} | _] -> 4;          % three of a kind
        _ -> 2                      % one pair
      end;
    _ ->
      case CardCountList of
        [{_, 5} | _] -> 7;          % five of a kind
        [{_, 4} | _] -> 6;          % four of a kind
        [{_, 3}, {_, 2}] -> 5;      % full house
        [{_, 3} | _ ] -> 4;         % three of a kind
        [{_, 2}, {_, 2} | _] -> 3;  % two pairs
        [{_, 2} | _] -> 2;          % one pair
        _ -> 1                      % high card
      end
  end.

getHandCardCountMap(Cards) ->
  lists:foldl(
    fun(Card, Map) -> Map#{Card => maps:get(Card, Map, 0) + 1} end,
    #{},
    Cards
  ).

getSortedHandCardCountList(CardsMap) ->
  CardCountList = maps:to_list(CardsMap),
  lists:sort(
    fun({_, CountLeft}, {_, CountRight}) -> CountLeft > CountRight end,
    CardCountList
  ).

parseCard(Card) ->
  CardChar = charToString(Card),
  case CardChar of
    "J" -> 1;
    "2" -> 2;
    "3" -> 3;
    "4" -> 4;
    "5" -> 5;
    "6" -> 6;
    "7" -> 7;
    "8" -> 8;
    "9" -> 9;
    "T" -> 10;
    "Q" -> 11;
    "K" -> 12;
    "A" -> 13;
    _ -> throw({error, io_lib:format("Unsupported value: ~p", [CardChar])})
  end.

parseCards(Cards) ->
  lists:map(fun parseCard/1, Cards).

parseHand(Line) ->
  [Cards, Bet] = string:lexemes(string:trim(Line), " "),
  {BetAmount, _} = string:to_integer(Bet),
  CardValues = parseCards(Cards),
  #hand{
    bet = BetAmount,
    cards = CardValues,
    handValue = computeHandValue(CardValues)
  }.

readLines() -> readLines([]).

readLines(Lines) ->
  case string:trim(io:get_line("")) of 
    eof -> Lines;
    "" -> Lines;
    Line -> readLines(Lines ++ [Line])
  end.

start() ->
  InputLines = readLines(),
  Hands = lists:map(fun parseHand/1, InputLines),
  SortedHands = lists:sort(fun compareHands/2, Hands),
  SortedHandsWithIndices = lists:enumerate(SortedHands),
  PointsList = lists:map(fun({Index, Hand}) -> Index * Hand#hand.bet end, SortedHandsWithIndices),
  Points = lists:sum(PointsList),
  io:format("Points: ~p~n", [Points]).
