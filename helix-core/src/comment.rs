//! This module contains the functionality toggle comments on lines over the selection
//! using the comment character defined in the user's `languages.toml`

use once_cell::sync::Lazy;
use regex::Regex;

use crate::{
    find_first_non_whitespace_char, selection, Change, Range, Rope, RopeSlice, Selection, Tendril,
    Transaction,
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

#[derive(Debug, PartialEq)]
struct CommentChange {
    range: Range,
    open_pos: usize,
    close_pos: usize,
    open_margin: usize,
    close_margin: usize,
}

fn find_block_comments(
    open: &str,
    close: &str,
    text: RopeSlice,
    selection: &Selection,
) -> (bool, Vec<CommentChange>) {
    let mut commented = true;
    let mut comment_changes = Vec::with_capacity(selection.len());
    for range in selection {
        let selection_slice = range.slice(text);
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
            let open_margin = match selection_slice.get_char(open_pos + open.len()) {
                Some(c) if c == ' ' => 1,
                _ => 0,
            };

            let close_margin = if open_pos + open.len()
                != close_pos - std::cmp::min(close.len(), close_pos)
            {
                match selection_slice.get_char(close_pos - std::cmp::min(close.len(), close_pos)) {
                    Some(c) if c == ' ' => 1,
                    _ => 0,
                }
            } else {
                0
            };

            if !(open_fragment == open && close_fragment == close) {
                // as soon as one of the selections doesn't have a comment, only uncommented selections
                // should be changed.
                if commented {
                    comment_changes.clear();
                }
                comment_changes.push(CommentChange {
                    range: *range,
                    open_pos,
                    close_pos,
                    open_margin,
                    close_margin,
                });
                commented = false;
            } else if commented {
                comment_changes.push(CommentChange {
                    range: *range,
                    open_pos,
                    close_pos,
                    open_margin,
                    close_margin,
                });
            }
        }
    }
    if comment_changes.is_empty() {
        commented = false;
    }
    (commented, comment_changes)
}

#[must_use]
pub fn toggle_block_comments(
    doc: &Rope,
    selection: &Selection,
    tokens: Option<(&str, &str)>,
) -> Transaction {
    let (open_token, close_token) = tokens.unwrap_or(("/*", "*/"));
    let text = doc.slice(..);
    let (commented, comment_changes) =
        find_block_comments(open_token, close_token, text, selection);
    let open = Tendril::from(format!("{} ", open_token));
    let close = Tendril::from(format!(" {}", close_token));
    let mut changes: Vec<Change> = Vec::with_capacity(selection.len());
    for CommentChange {
        range,
        open_pos,
        close_pos,
        open_margin,
        close_margin,
    } in comment_changes
    {
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

pub fn split_lines_of_selection(text: RopeSlice, selection: &Selection) -> Selection {
    #[allow(clippy::trivial_regex)]
    static REGEX: Lazy<Regex> =
        Lazy::new(|| Regex::new(r"\r\n|[\n\r\u{000B}\u{000C}\u{0085}\u{2028}\u{2029}]").unwrap());

    selection::split_on_matches(
        text,
        &selection.clone().transform(|range| {
            // extend each line
            let (start_line, end_line) = range.line_range(text.slice(..));
            let start = text.line_to_char(start_line);
            let end = text.line_to_char((end_line + 1).min(text.len_lines()));

            Range::new(start, end).with_direction(range.direction())
        }),
        &REGEX,
    )
}

pub fn toggle_block_comments_as_line_fallback(
    text: &Rope,
    selection: &Selection,
    tokens: Option<(&str, &str)>,
) -> Transaction {
    toggle_block_comments(
        text,
        &split_lines_of_selection(text.slice(..), selection),
        tokens,
    )
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
    let mut lines: Vec<usize> = Vec::with_capacity(selection.len());
    let mut min_next_line = 0;
    for selection in selection {
        let (start, end) = selection.line_range(text);
        let start = start.max(min_next_line).min(text.len_lines());
        let end = (end + 1).min(text.len_lines());

        lines.extend(start..end);
        min_next_line = end;
    }
    let split_lines = split_lines_of_selection(text, selection);
    let (line_commented, block_commented) = match (token, tokens) {
        (Some(_), Some(tokens)) => (
            find_block_comments(tokens.0, tokens.1, text, &split_lines).0,
            find_block_comments(tokens.0, tokens.1, text, selection).0,
        ),
        (None, None) => (
            find_block_comments("/*", "*/", text, &split_lines).0,
            find_block_comments("/*", "*/", text, selection).0,
        ),
        (Some(_), None) => return CommentType::Line,
        (None, Some(tokens)) => (
            find_block_comments(tokens.0, tokens.1, text, &split_lines).0,
            find_block_comments(tokens.0, tokens.1, text, selection).0,
        ),
    };

    if line_commented {
        return CommentType::BlockAsLineFallback;
    }
    if block_commented {
        return CommentType::Block;
    }
    match (token, tokens) {
        (None, Some(_)) => CommentType::BlockAsLineFallback,
        _ => CommentType::Line,
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

    #[test]
    fn test_find_block_comments() {
        // three lines 5 characters.
        let mut doc = Rope::from("1\n2\n3");
        // select whole document
        let selection = Selection::single(0, doc.len_chars());

        let text = doc.slice(..);

        let res = find_block_comments("/*", "*/", text, &selection);

        assert_eq!(
            res,
            (
                false,
                vec![CommentChange {
                    range: Range::new(0, 5),
                    open_pos: 0,
                    close_pos: 4,
                    open_margin: 0,
                    close_margin: 0
                }]
            )
        );

        // comment
        let transaction = toggle_block_comments(&doc, &selection, None);
        transaction.apply(&mut doc);

        assert_eq!(doc, "/* 1\n2\n3 */");

        // uncomment
        let selection = Selection::single(0, doc.len_chars());
        let transaction = toggle_block_comments(&doc, &selection, None);
        transaction.apply(&mut doc);
        assert_eq!(doc, "1\n2\n3");

        // don't panic when there is just a space in comment
        doc = Rope::from("/* */");
        let selection = Selection::single(0, doc.len_chars());
        let transaction = toggle_block_comments(&doc, &selection, None);
        transaction.apply(&mut doc);
        assert_eq!(doc, "");
    }
}
