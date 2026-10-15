defmodule SymbrellaWeb.ChatLive.SourceLinksTest do
  use SymbrellaWeb.ConnCase, async: true

  import Phoenix.LiveViewTest

  alias SymbrellaWeb.ChatLive.{HTML, SourceLinks}

  @wikipedia "https://en.wikipedia.org/wiki/Maple_Ridge%2C_British_Columbia"
  @wikidata "https://www.wikidata.org/wiki/Q24639"

  test "formats source links without changing surrounding explanation" do
    reply = "According to Wikipedia, Maple Ridge is in British Columbia.\n" <>
              "Source: #{@wikipedia}"

    assert [
      %{kind: :text, text: "According to Wikipedia, Maple Ridge is in British Columbia.\n"},
      %{kind: :source, url: @wikipedia, favicon: "https://en.wikipedia.org/favicon.ico"}
    ] = SourceLinks.parts(reply)

    html = render_component(&HTML.assistant_text/1, message: %{text: reply})

    assert html =~ ~s(href="#{@wikipedia}")
    assert html =~ ~s(src="https://en.wikipedia.org/favicon.ico")
    assert html =~ ">Source</a>"
    assert html =~ "Maple Ridge is in British Columbia."
    assert html =~ ~s(rel="noopener noreferrer")
    refute html =~ ">Source: https://"
  end

  test "separate favicons for multiple source links with trailing punctuation" do
    reply = "- Wikipedia: Source: #{@wikipedia}\n" <>
              "- Wikidata: Source: #{@wikidata}."
    links = SourceLinks.parts(reply) |> Enum.filter(&(&1.kind == :source))

    assert Enum.map(links, & &1.url) == [@wikipedia, @wikidata]
    assert Enum.map(links, & &1.favicon) == [
      "https://en.wikipedia.org/favicon.ico",
      "https://www.wikidata.org/favicon.ico"
    ]

    html = render_component(&HTML.assistant_text/1, message: %{text: reply})
    assert html =~ ~s(href="#{@wikidata}")
    assert html =~ ~s(href="#{@wikipedia}")
    assert length(String.split(html, ">Source</a>")) == 3
    assert html =~ "Wikidata:"
  end

  test "distance answer renders tightly spaced inline citation links" do
    reply = "Based on Wikidata coordinates, the cities are 229.0 km apart. " <>
              "Sources: #{@wikipedia} and #{@wikidata}"
    html = render_component(&HTML.assistant_text/1, message: %{text: reply})

    assert html =~ ~s(href="#{@wikipedia}")
    assert html =~ ~s(href="#{@wikidata}")
    assert length(String.split(html, ">Source</a>")) == 3

    # Prevent accidental line breaks/indentation within a whitespace-pre-wrap
    # reply, which made two citations occupy several additional lines.
    text =
      html
      |> Floki.parse_fragment!()
      |> Floki.find("p")
      |> Floki.text()

    assert text ==
             "Based on Wikidata coordinates, the cities are 229.0 km apart. " <>
               "Sources: Source and Source"

    assert length(Floki.find(Floki.parse_fragment!(html), "p > a > img")) == 2
  end

  test "bare URLs are labeled Source and original punctuation remains visible" do
    assert [
      %{kind: :text, text: "See "},
      %{kind: :source, url: @wikipedia},
      %{kind: :text, text: ", and then continue."}
    ] = SourceLinks.parts("See #{@wikipedia}, and then continue.")
  end

  test "adjacent punctuation and trailing prose remain one text segment" do
    assert [
      %{kind: :text, text: "See "},
      %{kind: :source, url: @wikipedia},
      %{kind: :text, text: ", and then continue!"}
    ] = SourceLinks.parts("See #{@wikipedia}, and then continue!")
  end

  test "rejects credentials and disallowed schemes; escapes all other markup" do
    unsafe = ~S|<img src=x onerror="alert(1)"> Source: javascript:alert(1)|
    assert [%{kind: :text, text: ^unsafe}] = SourceLinks.parts(unsafe)

    credentials = "Source: https://someone:secret@example.com/private"
    assert [%{kind: :text, text: ^credentials}] = SourceLinks.parts(credentials)

    html = render_component(&HTML.assistant_text/1, message: %{text: unsafe})
    assert html =~ "&lt;img"
    refute html =~ "<img src=x onerror="
    refute html =~ "href=\"javascript:"

    html = render_component(&HTML.assistant_text/1, message: %{text: credentials})
    refute html =~ "href=\"https://someone:secret@example.com"
  end

  test "assistant bubble hides lexical tail without mutating the original text" do
    original = "Hi. Source: #{@wikipedia}\nBy the way, technical lexical output"
    html = render_component(&HTML.assistant_text/1, message: %{text: original})
    assert html =~ ">Source</a>"
    refute html =~ "technical lexical output"
    assert original =~ "By the way,"
  end

  test "non-URL assistant text is still rendered as escaped plain text" do
    assert [%{kind: :text, text: "Nothing to cite."}] =
      SourceLinks.parts("Nothing to cite.")

    html = render_component(&HTML.assistant_text/1, message: %{text: "No references."})
    assert html =~ "No references."
    refute html =~ "<a "
  end
end
