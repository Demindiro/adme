pub(crate) fn as_u8<T>(s: &[T]) -> &[u8] {
	// SAFETY:
	// * A &[u8] slice from any &[T] slice will be properly aligned.
	// * The length is trivially determined and cannot overflow.
	unsafe {
		core::slice::from_raw_parts(
			s.as_ptr().cast(),
			s.len() * core::mem::size_of::<T>(),
		)
	}
}

pub(crate) fn as_u8_mut<T>(s: &mut [T]) -> &mut [u8] {
	// SAFETY:
	// * A &[u8] slice from any &[T] slice will be properly aligned.
	// * The length is trivially determined and cannot overflow.
	unsafe {
		core::slice::from_raw_parts_mut(
			s.as_mut_ptr().cast(),
			s.len() * core::mem::size_of::<T>(),
		)
	}
}

