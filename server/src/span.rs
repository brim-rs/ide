// Adopted from https://github.com/brendanzab/codespan/blob/master/codespan-lsp/src/lib.rs

use brim::file::FileId;
use brim::files::{Files, FilesError, SimpleFiles};
use tower_lsp::lsp_types::{Position, Range};

fn location_to_position(
    line_str: &str,
    line: usize,
    column: usize,
    byte_index: usize,
) -> Result<Position, FilesError> {
    if column > line_str.len() {
        let max = line_str.len();
        let given = column;

        Err(FilesError::ColumnTooLarge { given, max })
    } else if !line_str.is_char_boundary(column) {
        let given = byte_index;

        Err(FilesError::InvalidCharBoundary { given })
    } else {
        let line_utf16 = line_str[..column].encode_utf16();
        let character = line_utf16.count() as u32;
        let line = line as u32;

        Ok(Position { line, character })
    }
}

pub fn byte_index_to_position(
    files: &SimpleFiles,
    file_id: usize,
    byte_index: usize,
) -> Result<Position, FilesError> {
    let source = files.source(file_id)?;

    let line_index = files.line_index(file_id, byte_index)?;
    let line_span = files.line_range(file_id, line_index).unwrap();

    let line_str = source
        .get(line_span.clone())
        .ok_or_else(|| FilesError::IndexTooLarge {
            given: if line_span.start >= source.len() {
                line_span.start
            } else {
                line_span.end
            },
            max: source.len() - 1,
        })?;
    let column = byte_index - line_span.start;

    location_to_position(line_str, line_index, column, byte_index)
}

pub fn byte_span_to_range(
    files: &SimpleFiles,
    file_id: usize,
    span: core::ops::Range<usize>,
) -> Result<Range, FilesError> {
    Ok(Range {
        start: byte_index_to_position(files, file_id, span.start)?,
        end: byte_index_to_position(files, file_id, span.end)?,
    })
}
