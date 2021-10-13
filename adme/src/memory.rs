pub trait Memory {
	fn load_u8(&mut self, address: u32) -> Result<u8, LoadError>;

	fn store_u8(&mut self, address: u32, value: u8) -> Result<(), StoreError>;

	fn load_u32(&mut self, address: u32) -> Result<u32, LoadError>;
	
	fn store_u32(&mut self, address: u32, value: u32) -> Result<(), StoreError>;
}

#[derive(Debug)]
pub enum LoadError {

}

#[derive(Debug)]
pub enum StoreError {

}
