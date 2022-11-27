//! This module contains the functionality toggle comments on lines over the selection
//! using the comment character defined in the user's `languages.toml`

use crate::{
    find_first_non_whitespace_char, Change, Range, Rope, RopeSlice, Selection, Tendril, Transaction,
};
use std::borrow::Cow;

/// Given text, a comment token, and a set of line indices, returns the following:
/// - Whether the given lines should be considered commented
///     - If any of the lines are uncommented, all lines are considered as such.
/// - The lines to change for toggling comments
///     - This is all provided lines excluding blanks lines.
/// - The column of the comment tokens
///     - Column of existing tokens, if the lines are commented; column to place tokens at otherwise.
/// - The margin to the right of the comment tokens
///     - Defaults to `1`. If any existing comment token is not followed by a space, changes to `0`.
fn find_line_comment(
    token: &str,
    text: RopeSlice,
    lines: impl IntoIterator<Item = usize>,
) -> (bool, Vec<usize>, usize, usize) {
    let mut commented = true;
    let mut to_change = Vec::new();
    let mut min = usize::MAX; // minimum col for find_first_non_whitespace_char
    let mut margin = 1;
    let token_len = token.chars().count();
    for line in lines {
        let line_slice = text.line(line);
        if let Some(pos) = find_first_non_whitespace_char(line_slice) {
            let len = line_slice.len_chars();

            if pos < min {
                min = pos;
            }

            // line can be shorter than pos + token len
            let fragment = Cow::from(line_slice.slice(pos..std::cmp::min(pos + token.len(), len)));

            if fragment != token {
                // as soon as one of the non-blank lines doesn't have a comment, the whole block is
                // considered uncommented.
                commented = false;
            }

            // determine margin of 0 or 1 for uncommenting; if any comment token is not followed by a space,
            // a margin of 0 is used for all lines.
            if !matches!(line_slice.get_char(pos + token_len), Some(c) if c == ' ') {
                margin = 0;
            }

            // blank lines don't get pushed.
            to_change.push(line);
        }
    }
    (commented, to_change, min, margin)
}

#[must_use]
pub fn toggle_line_comments(doc: &Rope, selection: &Selection, token: Option<&str>) -> Transaction {
    let text = doc.slice(..);

    let token = token.unwrap_or("//");
    let comment = Tendril::from(format!("{} ", token));

    let mut lines: Vec<usize> = Vec::with_capacity(selection.len());

    let mut min_next_line = 0;
    for selection in selection {
        let (start, end) = selection.line_range(text);
        let start = start.clamp(min_next_line, text.len_lines());
        let end = (end + 1).min(text.len_lines());

        lines.extend(start..end);
        min_next_line = end;
    }

    let (commented, to_change, min, margin) = find_line_comment(token, text, lines);

    let mut changes: Vec<Change> = Vec::with_capacity(to_change.len());

    for line in to_change {
        let pos = text.line_to_char(line) + min;

        if !commented {
            // comment line
            changes.push((pos, pos, Some(comment.clone())));
        } else {
            // uncomment line
            changes.push((pos, pos + token.len() + margin, None));
        }
    }

    Transaction::change(doc, changes.into_iter())
}

fn find_last_non_whitespace_char(text: RopeSlice) -> Option<usize> {
    let len = text.chars().len();
    for i in (0..len).rev() {
        if !text.get_char(i)?.is_whitespace() {
            return Some(i);
        }
    }
    None
}

type ToChange = Vec<(Range, usize, usize, usize, usize)>;
/// Return if all selections are block commented and which selections to toggle comments on
///
/// ToChange is a Vec of tuples representing where comments need to be either inserted or removed
/// the first item in each tuple is a Range the second and third item are the positions of the first and last
/// non whitespace char which is either the position of the open and close comment tokens if commented or the position
/// of the start and end of the text where comment tokens should be inserted. The fourth and fifth item are the margin
/// between the tokens and the text 1 if there is a space 0 if there is no space
fn find_block_comments(
    open: &str,
    close: &str,
    text: RopeSlice,
    selection: &Selection,
) -> (bool, ToChange) {
    let mut commented = true;
    let mut to_change: ToChange = Vec::with_capacity(selection.len());
    for selection in selection {
        let selection_slice = selection.slice(text);
        if let (Some(open_pos), Some(close_pos)) = (
            find_first_non_whitespace_char(selection_slice),
            find_last_non_whitespace_char(selection_slice),
        ) {
            let len = selection_slice.len_chars();

            // selection can be shorter than open_pos + open len
            let open_fragment = Cow::from(
                selection_slice.slice(open_pos..std::cmp::min(open_pos + open.len(), len)),
            );
            let close_fragment = Cow::from(
                selection_slice
                    .slice(close_pos - std::cmp::min(close.len() - 1, close_pos)..close_pos + 1),
            );

            // margin of one if there is a space between the comment token and other characters
            let open_margin = if matches!(selection_slice.get_char(open_pos + open.len()), Some(c) if c == ' ')
            {
                1
            } else {
                0
            };
            let close_margin = if matches!(selection_slice.get_char(close_pos - std::cmp::min(close.len(), close_pos)), Some(c) if c == ' ')
            {
                1
            } else {
                0
            };

            if !(open_fragment == open && close_fragment == close) {
                // as soon as one of the selections doesn't have a comment, only uncommented selections
                // should be changed.
                if commented {
                    to_change.clear();
                }
                to_change.push((*selection, open_pos, close_pos, open_margin, close_margin));
                commented = false;
            } else if commented {
                to_change.push((*selection, open_pos, close_pos, open_margin, close_margin));
            }
        }
    }
    if to_change.is_empty() {
        commented = false;
    }
    (commented, to_change)
}

#[must_use]
pub fn toggle_block_comments(
    doc: &Rope,
    selection: &Selection,
    tokens: Option<(&str, &str)>,
) -> Transaction {
    let (open_token, close_token) = tokens.unwrap_or(("/*", "*/"));
    let text = doc.slice(..);
    let (commented, to_change) = find_block_comments(open_token, close_token, text, selection);
    let open = Tendril::from(format!("{} ", open_token));
    let close = Tendril::from(format!(" {}", close_token));
    let mut changes: Vec<Change> = Vec::with_capacity(selection.len());
    for (range, open_pos, close_pos, open_margin, close_margin) in to_change {
        let from = range
            .with_direction(crate::movement::Direction::Forward)
            .from();
        if commented {
            changes.push((
                from + open_pos,
                from + open_pos + open_token.len() + open_margin,
                None,
            ));
            changes.push((
                from + close_pos - close_token.len() - close_margin + 1,
                from + close_pos + 1,
                None,
            ));
        } else {
            changes.push((from + open_pos, from + open_pos, Some(open.clone())));
            changes.push((
                from + close_pos + 1,
                from + close_pos + 1,
                Some(close.clone()),
            ));
        }
    }
    Transaction::change(doc, changes.into_iter())
}

pub enum CommentType {
    Line,
    Block,
    BlockAsLineFallback,
}

/// Return what CommentType selection should be toggled with
pub fn comment_type(
    token: Option<&str>,
    tokens: Option<(&str, &str)>,
    text: RopeSlice,
    selection: &Selection,
) -> CommentType {
    let max_line_length = selection
        .ranges()
        .iter()
        .map(|range| {
            let range = range.with_direction(crate::movement::Direction::Forward);
            range.line_range(text).1 - range.line_range(text).0 + 1
        })
        .max();

    let (token, tokens) = match (token, tokens, max_line_length) {
        (Some(token), Some(tokens), _) => (token, tokens),
        (None, None, _) => ("//", ("/*", "*/")),
        (Some(_), None, _) => return CommentType::Line,
        (None, Some(_), Some(1)) => return CommentType::BlockAsLineFallback,
        (None, Some(_), _) => return CommentType::Block,
    };
    let mut lines: Vec<usize> = Vec::with_capacity(selection.len());
    let mut min_next_line = 0;
    for selection in selection {
        let (start, end) = selection.line_range(text);
        let start = start.max(min_next_line).min(text.len_lines());
        let end = (end + 1).min(text.len_lines());

        lines.extend(start..end);
        min_next_line = end;
    }

    let (line_commented, _, _, _) = find_line_comment(token, text, lines);
    let (block_commented, _) = find_block_comments(tokens.0, tokens.1, text, selection);

    if line_commented {
        return CommentType::Line;
    }
    if block_commented {
        return CommentType::Block;
    }

    // line comment if all ranges are one line long
    match max_line_length {
        Some(1) => CommentType::Line,
        _ => CommentType::Block,
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_find_line_comment() {
        // four lines, two space indented, except for line 1 which is blank.
        let mut doc = Rope::from("  1\n\n  2\n  3");
        // select whole document
        let mut selection = Selection::single(0, doc.len_chars() - 1);

        let text = doc.slice(..);

        let res = find_line_comment("//", text, 0..3);
        // (commented = true, to_change = [line 0, line 2], min = col 2, margin = 0)
        assert_eq!(res, (false, vec![0, 2], 2, 0));

        // comment
        let transaction = toggle_line_comments(&doc, &selection, None);
        transaction.apply(&mut doc);
        selection = selection.map(transaction.changes());

        assert_eq!(doc, "  // 1\n\n  // 2\n  // 3");

        // uncomment
        let transaction = toggle_line_comments(&doc, &selection, None);
        transaction.apply(&mut doc);
        selection = selection.map(transaction.changes());
        assert_eq!(doc, "  1\n\n  2\n  3");
        assert!(selection.len() == 1); // to ignore the selection unused warning

        // 0 margin comments
        doc = Rope::from("  //1\n\n  //2\n  //3");
        // reset the selection.
        selection = Selection::single(0, doc.len_chars() - 1);

        let transaction = toggle_line_comments(&doc, &selection, None);
        transaction.apply(&mut doc);
        selection = selection.map(transaction.changes());
        assert_eq!(doc, "  1\n\n  2\n  3");
        assert!(selection.len() == 1); // to ignore the selection unused warning

        // 0 margin comments, with no space
        doc = Rope::from("//");
        // reset the selection.
        selection = Selection::single(0, doc.len_chars() - 1);

        let transaction = toggle_line_comments(&doc, &selection, None);
        transaction.apply(&mut doc);
        selection = selection.map(transaction.changes());
        assert_eq!(doc, "");
        assert!(selection.len() == 1); // to ignore the selection unused warning

        // TODO: account for uncommenting with uneven comment indentation
    }
}
