//! This module contains the functionality toggle comments on lines over the selection
//! using the comment character defined in the user's `languages.toml`

use smallvec::SmallVec;

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
    text.chars_at(text.len_chars())
        .reversed()
        .position(|ch| !ch.is_whitespace())
        .map(|pos| text.len_chars() - pos - 1)
}

#[derive(Debug, PartialEq, Eq)]
pub struct CommentChange {
    range: Range,
    open_pos: usize,
    close_pos: usize,
    open_margin: bool,
    close_margin: bool,
}

pub fn find_block_comments(
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
            let len = (close_pos + 1) - open_pos;
            let open_len = open.chars().count();
            let close_len = close.chars().count();
            let after_open = open_pos + open_len;
            let before_close = close_pos.saturating_sub(close_len);

            let line_commented = if len >= open_len + close_len {
                let open_fragment = selection_slice.slice(open_pos..after_open);
                let close_fragment = selection_slice.slice(before_close + 1..close_pos + 1);

                open_fragment == open && close_fragment == close
            } else {
                false
            };

            if !line_commented {
                // as soon as one of the selections doesn't have a comment, only uncommented selections
                // should be changed.
                if commented {
                    comment_changes.clear();
                }
                comment_changes.push(CommentChange {
                    range: *range,
                    open_pos,
                    close_pos,
                    open_margin: false,
                    close_margin: false,
                });
                commented = false;
            } else if commented {
                comment_changes.push(CommentChange {
                    range: *range,
                    open_pos,
                    close_pos,
                    open_margin: selection_slice
                        .get_char(after_open)
                        .map_or(false, |c| c == ' '),
                    close_margin: after_open != before_close
                        && selection_slice
                            .get_char(before_close)
                            .map_or(false, |c| c == ' '),
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
pub fn create_block_comment_transaction(
    doc: &Rope,
    selection: &Selection,
    tokens: Option<(&str, &str)>,
    commented: bool,
    comment_changes: Vec<CommentChange>,
) -> Transaction {
    let (open_token, close_token) = tokens.unwrap_or(("/*", "*/"));
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
                from + open_pos + open_token.len() + open_margin as usize,
                None,
            ));
            changes.push((
                from + close_pos - close_token.len() - close_margin as usize + 1,
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
    create_block_comment_transaction(doc, selection, tokens, commented, comment_changes)
}

pub fn split_lines_of_selection(text: RopeSlice, selection: &Selection) -> Selection {
    let mut ranges = SmallVec::new();
    for range in selection.ranges() {
        let (line_start, line_end) = range.line_range(text.slice(..));
        let mut pos = text.line_to_char(line_start);
        for line in text.slice(pos..text.line_to_char(line_end + 1)).lines() {
            let start = pos;
            pos += line.len_chars();
            ranges.push(Range::new(start, pos));
        }
    }
    Selection::new(ranges, 0)
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
                    open_margin: false,
                    close_margin: false
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
