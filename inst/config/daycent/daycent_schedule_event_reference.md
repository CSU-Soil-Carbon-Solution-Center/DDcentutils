# DayCent Schedule Event Reference For QC

This artifact records schedule-event lookup rules and immediate input syntax that QC should use when validating `.sch` event lines.

## Event Vocabulary

The event list below is currently treated as complete. Unknown event names should be `STOP`.

Events requiring `.100` library lookup:

| Event | Library file | Lookup exception |
| --- | --- | --- |
| `CROP` | `crop.100` | none |
| `CULT` | `cult.100` | none |
| `FERT` | `fert.100` | immediate input mode, except embedded named option still needs lookup |
| `IRRI` | `irri.100` | immediate input mode |
| `OMAD` | `omad.100` | none |
| `GRAZ` | `graz.100` | none |
| `HARV` | `harv.100` | none |
| `FIRE` | `fire.100` | none |
| `TREE` | `tree.100` | none |
| `TREM` | `trem.100` | none |

Events not requiring library lookup:

```text
EROD
FST
FLOD
AFRT
DRAN
IRIG
PLTM
LAST
SENM
TFST
TLST
```

`IRIG` may use irrigation immediate input mode. It does not require `irri.100` lookup when the argument is parenthesized immediate input.

## Day-Of-Year Rules

Schedule event day-of-year values:

- `0` is `STOP`.
- `367+` is `STOP`.
- `366` is allowed. DayCent handles wraparound/out-of-place cases.

## Crop/Tree Pairing Rules

QC should enforce valid start/end pairings. Valid pair families:

```text
FRST:LAST
PLTM:LAST
TFST:TLST
```

Unpaired starts or ends outside these families should produce a crop/tree pairing finding.

Current severity: `STOP`. Pairing mismatches can produce wrong model behavior even when DayCent does not fail.

Implementation detail:

- Pairing is explicit and order-aware.
- `FRST` or `PLTM` opens a crop pair.
- `LAST` closes the currently open crop pair.
- `TFST` opens a tree pair.
- `TLST` closes the currently open tree pair.
- `LAST` before an open `FRST`/`PLTM`, `TLST` before `TFST`, nested starts, and unclosed starts are all invalid.

## Fertilizer Immediate Input Mode

`FERT` normally reads a named option from `fert.100`:

```text
1 100 FERT N15C
```

If the `FERT` argument is enclosed in parentheses, it is immediate input mode:

```text
1 100 FERT (3.7N,1.0F)
1 226 FERT (6.5N,8.60I)
```

Immediate `FERT` values correspond to `fert.100` parameters:

| Key | Meaning |
| --- | --- |
| `N` | Nitrogen amount, g N m-2 |
| `P` | Phosphorus amount, g P m-2 |
| `S` | Sulfur amount, g S m-2 |
| `F` | Fraction of N fertilizer as NH4-N |
| `T` | Fraction of N fertilizer as NO3-N |
| `I` | Nitrification inhibitor: `ninhtm.ninib I`, where `ninhtm` is weeks and `ninib` is reduction factor |

Without `F`, immediate fertilizer defaults to 80% NH4-N and 20% NO3-N.

Newer DDcentEVI versions allow immediate input to include a named option from `fert.100` inside the parentheses:

```text
1 100 FERT (3.7N,0.75F,0.2T,N15C)
```

QC behavior:

- If `FERT` argument is not parenthesized, lookup the argument in `fert.100`.
- If `FERT` argument is parenthesized and contains only numeric keyed immediate values, no `fert.100` lookup is needed.
- If `FERT` argument is parenthesized and contains a named option beginning with a letter, lookup that named option in `fert.100`.
- Immediate values override duplicated parameters from the named `fert.100` option.
- Library option-name matching is exact. Do not case-fold schedule arguments or library block IDs.

Suggested tokenizer:

- Parenthesized payload is split on commas and/or whitespace.
- Direct keyed values generally match a numeric value followed by one of `N`, `P`, `S`, `F`, `T`, or `I`.
- A payload token beginning with a letter is a named `fert.100` option candidate.

## Irrigation Immediate Input Mode

`IRRI` normally reads a named option from `irri.100`:

```text
1 80 IRRI DRYLAND
```

If the `IRRI` or `IRIG` argument is enclosed in parentheses, it is immediate input mode:

```text
1 80 IRIG (1,0.925F,100L)
1 80 IRIG (5C)
1 80 IRIG (3A 0.99F -1L)
1 80 IRRI (0 5)
1 80 IRRI (4A, 0.96F)
```

Immediate irrigation values can be delimited by key letters, spaces, or commas. Keyed values can be in any order. If keys are missing, values are read in `irri.100` order.

Immediate irrigation keys:

| Key | Parameter | Meaning |
| --- | --- | --- |
| `A` | `AUIRRI` | Irrigation type |
| `F` | `FAWHC` | Fraction of available soil water below which irrigation occurs |
| `C` | `IRRAMT` or `IRRAUT` | Irrigation amount in cm |
| `L` | `AINTVL` | Auto-irrigation interval |

`AUIRRI` values:

| Value | Meaning |
| --- | --- |
| `0` | Automatic irrigation off |
| `1` | Irrigate top 30 cm to field capacity |
| `2` | Irrigate with specified amount |
| `3` | Irrigate top 30 cm to field capacity plus PET |
| `4` | Irrigate rooting zone to field capacity |

`AINTVL` values:

| Value | Meaning |
| --- | --- |
| `1L` | Irrigate on scheduled day only, for `IRIG` |
| `xL` | Irrigate for `x` days where `x > 0`, for `IRIG` |
| `0L` | Stop automatic irrigation, for `IRRI` or `IRIG` |
| `-1L` | Irrigate until end of growing season, for `IRIG` |

QC behavior:

- If `IRRI` argument is not parenthesized, lookup the argument in `irri.100`.
- If `IRRI` argument is parenthesized, treat it as immediate input and do not require `irri.100` lookup.
- If `IRIG` argument is parenthesized, treat it as immediate input and do not require `irri.100` lookup.
- If `IRIG` has no parenthesized argument, it remains in the no-lookup event set unless future evidence says otherwise.
- Library option-name matching is exact. Do not case-fold schedule arguments or library block IDs.

## Duplicate Library IDs

Duplicate block IDs within one effective library file are `STOP`.

The same ID appearing across different library file types is allowed. For example, a block ID in `crop.100` and the same block ID in `cult.100` are independent.
