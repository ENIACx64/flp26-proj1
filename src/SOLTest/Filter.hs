-- | Filtering test cases by include and exclude criteria.
--
-- The filtering algorithm is a two-phase set operation:
--
-- 1. __Include__: if no include criteria are given, all tests are included;
--    otherwise only tests matching at least one include criterion are kept.
--
-- 2. __Exclude__: tests matching any exclude criterion are removed from the
--    included set.
module SOLTest.Filter
  ( filterTests,
    matchesCriterion,
    matchesAny,
    trimFilterId,
  )
where

import Data.Char (isSpace)
import SOLTest.Types

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Apply a 'FilterSpec' to a list of test definitions.
--
-- Returns a pair @(selected, filteredOut)@ where:
--
-- * @selected@ are the tests that passed both include and exclude checks.
-- * @filteredOut@ are the tests that were removed by filtering.
--
-- The union of @selected@ and @filteredOut@ always equals the input list.
--
-- FLP: Implement this function using @matchesAny@ and @matchesCriterion@.
filterTests ::
  FilterSpec ->
  [TestCaseDefinition] ->
  ([TestCaseDefinition], [TestCaseDefinition])
filterTests spec tests =
  -- \| Using let since multiple local definitions are needed
  let includes = fsIncludes spec
      excludes = fsExcludes spec
      useRegex = fsUseRegex spec
      -- \| Filters tests by 'fsIncludes' and stores tests that pass
      included =
        -- \| If 'fsIncludes' is empty, stores the whole list
        if null includes
          then
            tests
          -- \| Else filters
          else
            filter (matchesAny useRegex includes) tests
      -- \| Filters tests by 'fsExcludes' and stores tests that pass
      excluded = filter (matchesAny useRegex excludes) tests
      -- \| A predicate that returns True if a test is in the included list and not in the excluded list
      toBeSelected test = test `elem` included && test `notElem` excluded
      -- \| 'selected' and 'filteredOut' are the output fields (see description)
      selected = filter toBeSelected tests
      filteredOut = filter (not . toBeSelected) tests
   in (selected, filteredOut)

-- | Check whether a test matches at least one criterion in the list.
matchesAny :: Bool -> [FilterCriterion] -> TestCaseDefinition -> Bool
matchesAny useRegex criteria test =
  any (matchesCriterion useRegex test) criteria

-- | Check whether a test matches a single 'FilterCriterion'.
--
-- When @useRegex@ is 'False', matching is case-sensitive string equality.
-- When @useRegex@ is 'True', the criterion value is treated as a POSIX
-- regular expression matched against the relevant field(s).
--
-- FLP: Implement this function. If you're not implementing the regex matching
-- bonus extension, you can either remove the first argument and update the usages,
-- or you can simply ignore the value.
matchesCriterion :: Bool -> TestCaseDefinition -> FilterCriterion -> Bool
matchesCriterion useRegex test criterion =
  -- \| Using case to branch different options
  case criterion of
    -- \| Returns True if any match is found
    ByAny str -> str == tcdName test || str == tcdCategory test || str `elem` tcdTags test
    -- \| Returns True if a match by category is found
    ByCategory str -> str == tcdCategory test
    -- \| Returns True if a match by tags is found
    ByTag str -> str `elem` tcdTags test

-- | Trim leading and trailing whitespace from a filter identifier.
trimFilterId :: String -> String
trimFilterId = reverse . dropWhile isSpace . reverse . dropWhile isSpace
