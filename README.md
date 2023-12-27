# PokeChecker interactive shiny app.

Pokechecker enables you to check your PokéDex regarding special forms, males, females, three-star rated, shadow and purified Pokémon. It is based on a Google Sheet, which needs to be kept up-to-date because Niantic has not allowed to use any API to access your data automatically.

You can use your own Google Sheet. Note, however, that it is important that it is structured exactly like my own sheet available [here](https://docs.google.com/spreadsheets/d/1WZWY-zCCki9jD26-v7QQ1CCX85NmiYclPLukeIPHJV4/). Also, make sure to transfer the Pokémon names in columns "name_german" and "name_english" exactly as is, because the code expects those names. So, basically just copy the whole sheet and replace my x's with your x's for the Pokémon that are present in your Pokédex.

When you create your own Google Sheet, make sure that the Sheet whithin the file is called "pokedex". The filename within Google Drive does not matter. After creating the Google Sheet, set "General access" -\> "Anyone with the link" -\> "Anyone on the Internet with the link can view" so that the app has permission to access the sheet. Copy the link and paste it in the text field provided at the top of the app.

Everything is available in English and German. At start-up, the app defaults to my Google Sheet and to German. If you want to change that, you need to copy the code and deploy your own app.

Currently, the app is reachable on [my Posit Shiny server](https://peter-t-ruehr.shinyapps.io/PokeChecker/).

Issues can be reported at [PokeChecker's Issues page](https://github.com/Peter-T-Ruehr/PokeChecker/issues), where comments and ideas are also welcome. Enjoy!
