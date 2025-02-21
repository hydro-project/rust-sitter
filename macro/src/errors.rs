use std::marker::PhantomData;

/// An iterator that maps [`Result`]s to their [`Ok`] values
/// and stores combined errors within itself.
struct CollectingShunt<'a, I, A> {
    iter: I,
    err: &'a mut Option<syn::Error>,
    _marker: PhantomData<fn() -> A>,
}

impl<I, A> Iterator for CollectingShunt<'_, I, A>
where
    I: Iterator<Item = syn::Result<A>>,
{
    type Item = A;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iter.next() {
            Some(Ok(x)) => Some(x),
            Some(Err(another)) => {
                match self.err {
                    Some(x) => x.combine(another),
                    ref mut x => **x = Some(another),
                }
                None
            }
            _ => None,
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let (_, upper) = self.iter.size_hint();
        (0, upper)
    }
}

pub trait IteratorExt<A>: Iterator<Item = syn::Result<A>> {
    /// Reduces an iterator with items of type [`syn::Result<T>`] into one large collection,
    /// [combining] errors and [collecting] successes.
    ///
    /// [combining]: syn::Error::combine
    /// [collecting]: FromIterator
    fn sift<T>(self) -> syn::Result<T>
    where
        Self: Sized,
        T: FromIterator<A>,
    {
        let mut err = None;
        let iter = CollectingShunt {
            iter: self,
            err: &mut err,
            _marker: PhantomData,
        };
        let collection = iter.collect();
        match err {
            Some(error) => Err(error),
            None => Ok(collection),
        }
    }
}

impl<A, T> IteratorExt<A> for T where T: Iterator<Item = syn::Result<A>> {}
