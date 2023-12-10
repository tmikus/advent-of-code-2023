-module(main).
-export([parseCard/1, parseHand/1, start/0]).

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
  CardCountMap = getHandCardCountMap(Cards),
  CardCountList = getSortedHandCardCountList(CardCountMap),
  case CardCountList of
    [{_, 5}] -> 7;            % five of a kind
    [{_, 4}, {_, 1}] -> 6;    % four of a kind
    [{_, 3}, {_, 2}] -> 5;    % full house
    [{_, 3} | _ ] -> 4;        % three of a kind
    [{_, 2}, {_, 2} | _] -> 3; % two pairs
    [{_, 2} | _] -> 2;         % one pair
    _ -> 1                    % high card
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
    "2" -> 2;
    "3" -> 3;
    "4" -> 4;
    "5" -> 5;
    "6" -> 6;
    "7" -> 7;
    "8" -> 8;
    "9" -> 9;
    "T" -> 10;
    "J" -> 11;
    "Q" -> 12;
    "K" -> 13;
    "A" -> 14;
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
