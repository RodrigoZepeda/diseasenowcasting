# Using alongside an LLM (AI)

> **NOTE**
>
> When working with Large Language Models (LLMs) consider your
> organization’s guidelines as well as any laws regarding the usage of
> epidemic data.
>
> - **Online LLMs (ChatGPT, Claude, Gemini)** usually use your data to
>   train their models so a version of your data might become completely
>   public. Prompt them to help you create the nowcasting models **but
>   do not pass them real data** unless you are absolutely certain of
>   what you are doing (both technically and legally).
>
> - **Local LLMs (Llama, Mistral, Qwen)** can be run inside your
>   computer without the data ever leaving your system. They are less
>   powerful but help with handling data privately

## Pair-programming with an LLM (`SKILL.md`)

`diseasenowcasting` ships a reference written *for LLM assistants*:
[`SKILL.md`](https://github.com/RodrigoZepeda/diseasenowcasting/blob/master/SKILL.md).

This recipe is the same for any LLM. Put the contents of `SKILL.md` into
the assistant’s context, then prompt:

1.  **Get the file.** Open the link above and copy the text. Or download
    using `R`:

    ``` r

    skill_url <- "https://raw.githubusercontent.com/RodrigoZepeda/diseasenowcasting/master/SKILL.md"
    download.file(
      url = skill_url,
      destfile = "SKILL.md",
      method = "libcurl" 
    )
    ```

2.  **Hand it to the LLM**

    - **Claude** (claude.ai or Claude Code): attach `SKILL.md` or paste
      it. In Claude Code, drop it into the project and it loads as a
      skill automatically.
    - **ChatGPT**: paste `SKILL.md` into a *Project* / custom-GPT
      instruction, or attach the file to the conversation.
    - **Gemini**: paste `SKILL.md` into the prompt (or a *Gem*’s
      instructions).
    - **Local models** (Llama, Mistral, Qwen …): put `SKILL.md` in the
      **system prompt**. If the context window is small, paste only the
      sections you need (e.g. *“2. The model menu”*, *“3. Data
      preparation”*, *“4. Fitting”*).

3.  **Prompt.** For example using ChatGPT one can pass the prompt along
    the file:

        The file SKILL.md is an API reference for the `R` package
         `diseasenowcasting`. Using this API write R code that builds a 
         tbl_now from a weekly linelist with columns `onset`, `reported`, 
         and `region`, gits an HSGP with a negative binomial likelihood and a 
         generalized gamma delay and plot the results.

    And get as a response:

        <THIS ANSWER COMES FROM CHATGPT'S MODEL GPT-5.5 INSTANT AS OF JUNE 8TH 2026> 
        Based solely on the API in SKILL.md, the workflow is:

        1. Build a tbl_now from the weekly linelist.
        2. Define a model with:
            `nb_likelihood()`
            `hsgp_epidemic()`
            `generalized_gamma_delay()`
        3. Fit with `nowcast()`.
        4. Generate predictions with `predict()`.
        5. Plot with `autoplot()`.

        ```R
        library(diseasenowcasting)
        library(tbl.now)
        library(ggplot2)

        # weekly linelist with columns:
        # onset    : event date
        # reported : report date
        # region   : geographic stratum

        # Build tbl_now
        tn <- tbl_now(
          weekly_linelist,
          event_date = onset,
          report_date = reported,
          strata = region,
          data_type = "linelist",
          verbose = FALSE
        )

        # HSGP epidemic + Negative Binomial likelihood +
        # Generalized Gamma reporting delay
        mdl <- model(
          nb_likelihood(),
          hsgp_epidemic(),
          generalized_gamma_delay()
        )

        # Fit nowcast model
        nc <- nowcast(
          data = tn,
          model = mdl,
          n_draws = 2000
        )

        # Posterior predictive nowcast
        pred <- predict(nc)

        # Plot median and uncertainty ribbons
        autoplot(pred)

    \`\`\`
