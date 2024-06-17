defmodule SyntaxParser do
  @moduledoc """
  Documentation for SyntaxParser
  The syntax parser is a system that converts typed commands from the user into requests to the game server.
  """

  @doc """
  The tokenizer is a function that simply splits a string into a list of words, removing any whitespace or punctuation. This prepares the user input for parsing.
  """
  @spec tokenizer(charlist()) :: [charlist()]
  def tokenizer() do

  end

  @doc """
  The term filter is a function that removes prepositions and articles from the list of tokens.
  """
  @spec term_filter([charlist()]) :: [charlist()]
  def term_filter(tokens) do

  end

  @doc """
  The action thesaurus is a function that converts verb strings into valid action atoms. This allows for synonyms to be used in place of the standard form. For example, 'go' and 'move' are both string aliases for the 'move' action atom.
  If a verb is not recognized, the parser will assume it refers to an object or entity in the context.
  """
  @spec action_thesaurus(charlist()) ::
  def action_thesaurus(action_string) do

  end

end
