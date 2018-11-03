use engine::*;
use engine::vector_op::vector_operator::*;


#[derive(Debug)]
pub struct SlicePackString {
    pub input: BufferRef,
    pub output: BufferRef,
    pub stride: usize,
    pub offset: usize,
}

impl<'a> VecOperator<'a> for SlicePackString {
    fn execute(&mut self, _: bool, scratchpad: &mut Scratchpad<'a>) {
        let data = scratchpad.get::<&'a str>(self.input);
        let mut packed_any = scratchpad.get_any_mut(self.output);
        let packed = packed_any.cast_ref_mut_byte_slices();
        for (i, datum) in data.iter().enumerate() {
            packed.data[i * self.stride + self.offset] = datum.as_bytes();
        }
    }

    fn init(&mut self, _: usize, batch_size: usize, scratchpad: &mut Scratchpad<'a>) {
        if scratchpad.get_any(self.output).len() == 0 {
            scratchpad.set(self.output, Box::new(ByteSlices {
                row_len: self.stride,
                data: vec![&[]; batch_size * self.stride],
            }));
        }
    }

    fn inputs(&self) -> Vec<BufferRef> { vec![self.input] }
    fn outputs(&self) -> Vec<BufferRef> { vec![self.output] }
    fn can_stream_input(&self, _: BufferRef) -> bool { true }
    fn can_stream_output(&self, _: BufferRef) -> bool { true }
    fn allocates(&self) -> bool { true }

    fn display_op(&self, _: bool) -> String {
        format!("{}[{}, {}, ...] = {}", self.output, self.offset, self.offset + self.stride, self.input)
    }
}