package data.xml;

import java.io.StringReader;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;


/**
 * Generic XML processor
 * @author Ryan Danas
 *
 * @param <T> The class the processor should generate
 */
public class XmlProcessor<T>
{
	JAXBContext context;

	/**
	 * Give it a class as context, and it will attempt to parse any XML data into that class
	 * @param clazz class context, should match generic tag
	 * @throws JAXBException
	 */
	public XmlProcessor(Class<T> clazz) throws JAXBException
	{
		this.context = JAXBContext.newInstance(clazz);
	}
	
	// its either suppress the type-erasure warning in one place or cast everywhere this method is called
	@SuppressWarnings("unchecked")
	/**
	 * Processes an XML string into a class
	 * @param xml The XML data to unwrap into a class
	 * @return The class specified by the generic tag / context in the constructor
	 * @throws JAXBException The processor was unable to generate the class asked for with the given string
	 */
	public T unwrap(String xml) throws JAXBException
	{
		Unmarshaller unwrapper = context.createUnmarshaller();
		T object = (T) unwrapper.unmarshal(new StringReader(xml));
		return object;
	}
}